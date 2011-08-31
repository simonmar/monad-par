{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP, ParallelListComp,
             PatternGuards
	     #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- | This module exposes the internals of the @Par@ monad so that you
-- can build your own scheduler or other extensions.  Do not use this
-- module for purposes other than extending the @Par@ monad with new
-- functionality.

module Control.Monad.Par.Internal (
   Trace(..), Sched(..), Par(..),
   IVar(..), IVarContents(..),
   sched,
   runPar, runParAsync, runParAsyncHelper,
   new, newFull, newFull_, get, put_, put,
   pollIVar, yield,
 ) where


import Control.Monad as M hiding (sequence, join)
import Prelude hiding (mapM, sequence)
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent hiding (yield)
import GHC.Conc hiding (yield)
import Control.DeepSeq
import Control.Applicative
import Data.Array
--import Data.List (find)
--import Data.Maybe (catMaybes)
--import Text.Printf



-- ---------------------------------------------------------------------------
-- MAIN SCHEDULING AND RUNNING
-- ---------------------------------------------------------------------------

data Trace = forall a . Get (IVar a) (a -> Trace)
           | forall a . Put (IVar a) a Trace
           | forall a . New (IVarContents a) (IVar a -> Trace)
           | Fork Trace Trace
           | Done
           | Yield Trace

data Sched = Sched
  { no          :: {-# UNPACK #-} !Int,
        -- ^ The threadnumber of this worker
    workpool    :: IORef [Trace],
        -- ^ The workpool for this worker
    status      :: IORef AllStatus,
        -- ^ The status of all Scheds in this work set
    scheds      :: Array ThreadNumber Sched,
        -- ^ The list of all per-thread workers in this work set
    parent      :: Maybe Sched,
        -- ^ This worker's parent (or Nothing if this worker is top-level)
    checkChild  :: IORef [Sched],
        -- ^ List of children Scheds that may need work done
    founder     :: {-# UNPACK #-} !Int,
        -- ^ The creator of this work set
    complete    :: IORef Bool
        -- ^ True indicates that this work set is complete
#if __GLASGOW_HASKELL__ < 701 /* 20110301 */
        -- This is needed when threadCapability is not available to determine 
        -- the parent worker of new children
        -- See [Notes on threadCapability] for more details
    ,tId        :: IORef ThreadId
        -- ^ The ThreadId of this worker
#endif
  }

-- | The main scheduler loop.
sched :: Bool -> Sched -> Trace -> IO ()
sched _doSync queue t = loop t
 where 
  loop t = case t of
    New a f -> do
      r <- newIORef a
      loop (f (IVar r))
    Get (IVar v) c -> do
      e <- readIORef v
      case e of
         Full a -> loop (c a)
         _other -> do
           r <- atomicModifyIORef v $ \e -> case e of
                        Empty    -> (Blocked [c], reschedule queue)
                        Full a   -> (Full a,      loop (c a))
                        Blocked cs -> (Blocked (c:cs), reschedule queue)
           r
    Put (IVar v) a t  -> do
      cs <- atomicModifyIORef v $ \e -> case e of
               Empty    -> (Full a, [])
               Full _   -> error "multiple put"
               Blocked cs -> (Full a, cs)
      mapM_ (pushWork queue. ($a)) cs
      loop t
    Fork child parent -> do
         pushWork queue child
         loop parent
    Done ->
         if _doSync
         then reschedule queue
-- We could fork an extra thread here to keep numCapabilities workers
-- even when the main thread returns to the runPar caller...
         else do putStrLn " [par] Forking replacement thread..\n"
                 forkIO (reschedule queue); return ()
-- But even if we don't we are not orphaning any work in this
-- thread's work-queue because it can be stolen by other threads.
--	 else return ()

    Yield parent -> do 
        -- Go to the end of the worklist:
        let Sched { workpool } = queue
        -- TODO: Perhaps consider Data.Seq here.
        -- This would also be a chance to steal and work from opposite ends of the queue.
        atomicModifyIORef workpool $ \ts -> (ts++[parent], ())
        reschedule queue

-- | Process the next item on the work queue or, failing that, go into
--   work-stealing mode.
reschedule :: Sched -> IO ()
reschedule queue@Sched{ workpool, checkChild, no=my_no } = do
  -- First we check for children that have work to do
  mcc <- atomicPopIORef checkChild
  case mcc of
    Just c  -> do
        -- A child has work, so we'll enter it
        --printf "cpu %d switching to child\n" my_no
        atomicModifyIORef (status c) $ setMeWorking my_no
        updateMyWork my_no (Just c)
        reschedule c
    Nothing -> do
        e <- atomicPopIORef workpool
        case e of
            Just t  -> sched True queue t
            Nothing -> steal queue



-- RRN: Note -- NOT doing random work stealing breaks the traditional
-- Cilk time/space bounds if one is running strictly nested (series
-- parallel) programs.

-- | Attempt to steal work or, failing that, give up and go idle.
steal :: Sched -> IO ()
steal q@Sched{ status, scheds, no=my_no, parent, complete } = 
  -- printf "cpu %d stealing\n" my_no >>
  go l
  where
    (l,u) = bounds scheds
    go n
      | n > u = 
          case (founder q == my_no, parent) of
            (False, Just p) -> do
                    -- I'm not the founder and I have a parent - I'll be going absent
                sts <- atomicModifyIORef status $ setMeAbsent my_no
                updateMyWork my_no parent
                when (length sts == numCapabilities - 1) $
                    doShutDown complete sts
                --printf "cpu %d switching to parent\n" my_no
                reschedule p
            _ -> do
                    -- I'll be going idle
                m  <- newEmptyMVar
                sts <- atomicModifyIORef status $ setMeIdle my_no m
                updateMyWork my_no Nothing
                if length sts == numCapabilities - 1
                  then do
                    --printf "cpu %d shutting down (%s)\n" my_no 
                    --       (maybe "top-level" (const "founder") parent)
                    doShutDown complete sts
                  else do
                    finished <- readIORef complete
                    unless finished $ takeMVar m
                    finished <- readIORef complete
                    --printf "cpu %d waking up from truly idle and complete=%s\n" 
                    --       my_no (show finished)
                    updateMyWork my_no (Just q)
                    if finished
                      then do
                        --maybe (printf "cpu %d shutting down\n" my_no) 
                        --      (const $ printf "cpu %d shutting down child\n" my_no) parent
                        return ()
                      else do
                        --printf "cpu %d woken up\n" my_no
                        atomicModifyIORef status $ setMeWorking my_no
                        -- we might have been woken up by a child, so we 
                        -- should check our own work queue/children first
                        reschedule q
      | n == my_no = go (n+1)
      | otherwise  = do
         r <- atomicPopIORef $ workpool (scheds!n)
         case r of
           Just t  -> do
              -- printf "cpu %d got work from cpu %d\n" my_no (no x)
              sched True q t
           Nothing -> go (n+1)


-- ---------------------------------------------------------------------------
-- SCHEDULING UTILITY FUNCTIONS
-- ---------------------------------------------------------------------------

-- | Takes the "complete" ref from the scheduler and the list of non-working
-- scheduler statuses and shuts down the system if necessary
doShutDown :: IORef Bool -> [Status] -> IO ()
doShutDown complete sts = do
    sendKill <- atomicModifyIORef complete $
        \c -> if c then (c, False) else (True, True)
    when sendKill $ mapM_ (flip putMVar $ ()) [b | (Idle b) <- sts]

-- | If any worker is idle, wake one up and give it work to do.
pushWork :: Sched -> Trace -> IO ()
pushWork Sched { workpool, status, {-no=my_no,-} scheds } t = do
    atomicModifyIORef workpool $ \ts -> (t:ts, ())
    allstatus <- readIORef status
    when (getAlertableCount allstatus > 0) $ do
        r <- atomicModifyIORef status alertNonWorker
        case r of 
            Nothing -> return ()
            Just (n, st) -> do
                --printf "cpu %d alerting cpu %d\n" my_no n
                wakeUp (scheds!n) st -- wake it up

-- | wakeUp takes the given scheduler's status and wakes it up
wakeUp :: Sched -> Status -> IO ()
wakeUp _ (Idle b) = putMVar b () -- If it's idle, it's blocking, so we needn't alert the parent
wakeUp s Absent   = case parent s of
    Nothing -> return () -- This can happen right at the start
    Just p  -> do
        atomicModifyIORef (checkChild p) $ \cs -> (s:cs, ())
        i <- atomicModifyIORef (status p) $ alertThisNonWorker (no p)
        wakeUp p i
wakeUp _ _        = return ()


-- ---------------------------------------------------------------------------
-- OTHER UTILITY FUNCTIONS
-- ---------------------------------------------------------------------------

-- | A utility for popping an element off of an IORef list.
-- The return value is Just a where a is the head of the list
-- or Nothing if the list is null.
atomicPopIORef :: IORef [a] -> IO (Maybe a)
atomicPopIORef ref = atomicModifyIORef ref $ \lst ->
    case lst of
        []      -> ([], Nothing)
        (e:es)  -> (es, Just e)

-- | A utility function for updating a single element of an array.
-- TODO: Given that we do this a fair amount, we may want to use 
-- an array that can more efficiently update individual elements.
-- From the Array docs:
--    For most array types, this operation is O(/n/) where /n/ is the size
--    of the array.  However, the diffarray package provides an array type
--    for which this operation has complexity linear in the number of updates.
updateA :: Ix i => Array i e -> i -> e -> Array i e
updateA a i e = a // [(i,e)]


-- ---------------------------------------------------------------------------
-- THREAD STATUS
-- ---------------------------------------------------------------------------

data Status = Idle (MVar ()) -- The worker is idle and waiting to be released
            | Alerted        -- The worker has been alerted that work is available in this work set
            | Working        -- The worker is working in this work set or a child one
            | Absent         -- The worker is not in this work set
  deriving Eq

instance Show Status where
    show (Idle _) = "Idle"
    show Alerted  = "Alerted"
    show Working  = "Working"
    show Absent   = "Absent"
type AllStatus = (Int, Array ThreadNumber Status) -- a count of alertable workers and the array
type ThreadNumber = Int

-- | A new empty set of Statuses (defaulted to Absent)
newStatus :: Int -> AllStatus
newStatus n = (n, listArray (0, n-1) (repeat Absent))

-- | Returns the count of alertable workers
getAlertableCount :: AllStatus -> Int
getAlertableCount = fst

-- | Returns true if the given Status is alertable (Idle or Absent)
isAlertable :: Status -> Bool
isAlertable (Idle _) = True
isAlertable Absent   = True
isAlertable _        = False

-- | This searches the status list for an Idle or Absent thread and:
-- If it finds one, it sets it to Alerted and returns its thread number 
--  and previous status.
-- Otherwise, this changes nothing and returns Nothing.
-- 
-- TODO: Ideally, this should preferentially return Idle threads over Absent threads
alertNonWorker :: AllStatus -> (AllStatus, Maybe (Int, Status))
alertNonWorker (c, a) = 
    let (l,u) = bounds a
        go n | n > u                    = Nothing
             | s <- a!n, isAlertable s  = Just (n, s)
             | otherwise                = go (n+1)
        ret = go l
    in (maybe (c, a) (\(i, _) -> (c-1, updateA a i Alerted)) ret, ret)

-- | If the given thread's status is Idle or Absent, then it sets it to Alerted. 
-- Returns the original status
alertThisNonWorker :: ThreadNumber -> AllStatus -> (AllStatus, Status)
alertThisNonWorker n (c, a) = 
    if isAlertable s then ((c-1, updateA a n Alerted), s)
                     else ((c, a), s)
    where s = a!n

-- | Sets the given thread to Idle and returns a list of the blocking MVars of all
-- non-working threads.
setMeIdle :: ThreadNumber -> MVar () -> AllStatus -> (AllStatus, [Status])
setMeIdle n m (c, a) =
    ((if isAlertable (a!n) then c else c+1, updateA a n (Idle m)),
     [t | t <- elems a, t /= Working])

-- | Sets the given thread to Absent and returns a list of the blocking MVars of all
-- non-working threads.
setMeAbsent :: ThreadNumber -> AllStatus -> (AllStatus, [Status])
setMeAbsent n (c, a) =
    ((if isAlertable (a!n) then c else c+1, updateA a n Absent),
     [t | t <- elems a, t /= Working])

-- | Sets the given thread to working and returns nothing.
setMeWorking :: ThreadNumber -> AllStatus -> (AllStatus, ())
setMeWorking n (c, a) =
    ((if isAlertable (a!n) then c-1 else c, updateA a n Working), ())


-- ---------------------------------------------------------------------------
-- PAR AND IVAR
-- ---------------------------------------------------------------------------

newtype Par a = Par {
    runCont :: (a -> Trace) -> Trace
}

instance Functor Par where
    fmap f m = Par $ \c -> runCont m (c . f)

instance Monad Par where
    return a = Par ($ a)
    m >>= k  = Par $ \c -> runCont m $ \a -> runCont (k a) c

instance Applicative Par where
   (<*>) = ap
   pure  = return

newtype IVar a = IVar (IORef (IVarContents a))
-- data IVar a = IVar (IORef (IVarContents a))

data IVarContents a = Full a | Empty | Blocked [a -> Trace]

-- Forcing evaluation of a IVar is fruitless.
instance NFData (IVar a) where
  rnf _ = ()

-- From outside the Par computation we can peek.  But this is nondeterministic.
pollIVar :: IVar a -> IO (Maybe a)
pollIVar (IVar ref) = 
  do contents <- readIORef ref
     case contents of 
       Full x -> return (Just x)
       _      -> return (Nothing)


-- ---------------------------------------------------------------------------
-- GLOBAL THREAD IDENTIFICATION
-- ---------------------------------------------------------------------------

-- Global thread identification works by keeping two global variables.
-- The threadsMade variable indicates whether threads have already been made 
-- due to a call to runPar.
-- The currentWork variable references an array that contains the Scheds that 
-- the current running threads are working on.  If one of those threads 
-- encounters a runPar, it can index into this array to find out what the 
-- parent of the new work set should be.

-- TODO: This is not a robust system in its current implementation.

-- | This is the global variable indicating whether threads are available
threadsMade :: IORef Bool
threadsMade = unsafePerformIO $ newIORef False

-- | This is the global variable indicating what the current workers are doing
currentWork :: IORef (Array ThreadNumber (Maybe Sched))
currentWork = unsafePerformIO $ newIORef $ listArray (0, numCapabilities-1) (repeat Nothing)

-- | Gets the given thread's Sched out of the currentWork array
getCurrentSched :: ThreadNumber -> IO (Maybe Sched)
getCurrentSched n = liftM (!n) (readIORef currentWork)

-- | This needs to be called whenever a worker switches work sets.  This updates
-- the global currentWork to reflect that change.
updateMyWork :: ThreadNumber -> (Maybe Sched) -> IO ()
updateMyWork n newWork =
    atomicModifyIORef currentWork (\a -> (updateA a n newWork, ()))


-- ---------------------------------------------------------------------------
-- RUNPAR
-- ---------------------------------------------------------------------------

{-# INLINE runPar_internal #-}
runPar_internal :: Bool -> Par a -> a
runPar_internal _doSync x = unsafePerformIO $ do
    workpools <- replicateM numCapabilities $ newIORef []
    children  <- replicateM numCapabilities $ newIORef []
    status <- newIORef $ newStatus numCapabilities
    complete <- newIORef False

#if __GLASGOW_HASKELL__ >= 701 /* 20110301 */
    -- [Notes on threadCapability]
    --
    -- We create a thread on each CPU with forkOnIO.  The CPU on which
    -- the current thread is running will host the main thread; the
    -- other CPUs will host worker threads.
    --
    -- Note: GHC 7.1.20110301 is required for this to work, because that
    -- is when threadCapability was added.
    --
    (main_cpu, _) <- threadCapability =<< myThreadId
#else
    --
    -- Lacking threadCapability, we always pick CPU #0 to run the main
    -- thread.  If the current thread is not running on CPU #0, this
    -- will require some data to be shipped over the memory bus, and
    -- hence will be slightly slower than the version above.
    --
    -- If this is a nested runPar call, then we can look at the current 
    -- workers to figure out which one has a matching theadID.
    --
    cWork <- readIORef currentWork
    myTId <- myThreadId
    parentTIds <- M.mapM (\s -> (readIORef $ tId $ s) >>= (\t -> return (s,t)))
                         (catMaybes . elems $ cWork)
    main_cpu <- case find ((== myTId) . snd) parentTIds of
                    Nothing    -> {- printf "Failed to find a parent!\n" >> -} return 0
                    Just (s,_) -> return $ no s
#endif
    
    currentSched <- getCurrentSched main_cpu
    let parents = maybe (replicate numCapabilities Nothing) (map Just . elems . scheds) currentSched
#if __GLASGOW_HASKELL__ >= 701 /* 20110301 */
        -- See [Notes on threadCapability] for more details
    let states = listArray (0, numCapabilities-1)
                    [ Sched { no=n, workpool=wp, status, scheds=states, checkChild=c, 
                              parent=p, founder=main_cpu, complete }
                    | n <- [0..] | wp <- workpools | c <- children | p <- parents ]
#else
    tIds <- mapM (maybe (newIORef myTId) (\s -> newIORef =<< (readIORef . tId $ s))) parents
    let states = listArray (0, numCapabilities-1)
                    [ Sched { no=n, workpool=wp, status, scheds=states, checkChild=c, 
                              parent=p, founder=main_cpu, complete, tId=t }
                    | n <- [0..] | wp <- workpools | c <- children | p <- parents | t <- tIds ]
#endif

    m <- newEmptyMVar
    makeNewThreads <- atomicModifyIORef threadsMade $
                        \tm -> if tm then (tm, False) else (True, True)
    if makeNewThreads then do
      --printf "Making new threads\n"
      writeIORef currentWork (listArray (0, numCapabilities-1) (map Just (elems states)))
      forM_ (elems states) $ \s -> atomicModifyIORef status $ setMeWorking (no s)
      forM_ (elems states) $ \(state@Sched{no=cpu}) -> do
        forkOnIO cpu $ do
#if __GLASGOW_HASKELL__ < 701 /* 20110301 */
        -- Once again, see [Notes on threadCapability] for more details
          myTId <- myThreadId
          --printf "cpu %d setting threadId=%s\n" cpu (show myTId)
          writeIORef (tId state) myTId
#endif
          if (cpu /= main_cpu)
             then reschedule state
             else do
                  rref <- newIORef Empty
                  sched _doSync state $ runCont (x >>= put_ (IVar rref)) (const Done)
                  readIORef rref >>= putMVar m
      r <- takeMVar m
      --printf "done\n"
      
      -- TODO: If we're doing this nested strategy, we should probably just keep the 
      -- threads alive indefinitely.  After all, we can get some weird conditions 
      -- doing it this way.  At the least, we should put this in steal where the 
      -- shutdown occurs.
      writeIORef currentWork (listArray (0, numCapabilities-1) (repeat Nothing))
      writeIORef threadsMade False
      
      case r of
        Full a -> return a
        _ -> error "no result"

     else do
        --printf "Using old threads\n"
        let state = states ! main_cpu
        rref <- newIORef Empty
        atomicModifyIORef status $ setMeWorking main_cpu
        updateMyWork main_cpu (Just state)
        --printf "cpu %d starting in child\n" main_cpu
        sched _doSync state $ runCont (x >>= put_ (IVar rref)) (const Done)
        updateMyWork main_cpu (parent state)
        readIORef rref >>= putMVar m
        r <- takeMVar m
        --printf "cpu %d finished in child\n" main_cpu
        case r of
            Full a -> return a
            _ -> error "no result"

-- | The main way to run a Par computation
runPar :: Par a -> a
runPar = runPar_internal True

-- | An asynchronous version in which the main thread of control in a
-- Par computation can return while forked computations still run in
-- the background.  
runParAsync :: Par a -> a
runParAsync = runPar_internal False

-- | An alternative version in which the consumer of the result has
-- the option to "help" run the Par computation if results it is
-- interested in are not ready yet.
runParAsyncHelper :: Par a -> (a, IO ())
runParAsyncHelper = undefined -- TODO: Finish Me.


-- ---------------------------------------------------------------------------
-- PAR FUNCTIONS
-- ---------------------------------------------------------------------------

-- | creates a new @IVar@
new :: Par (IVar a)
new  = Par $ New Empty

-- | creates a new @IVar@ that contains a value
newFull :: NFData a => a -> Par (IVar a)
newFull x = deepseq x (Par $ New (Full x))

-- | creates a new @IVar@ that contains a value (head-strict only)
newFull_ :: a -> Par (IVar a)
newFull_ !x = Par $ New (Full x)

-- | read the value in a @IVar@.  The 'get' can only return when the
-- value has been written by a prior or parallel @put@ to the same
-- @IVar@.
get :: IVar a -> Par a
get v = Par $ \c -> Get v c

-- | like 'put', but only head-strict rather than fully-strict.
put_ :: IVar a -> a -> Par ()
put_ v !a = Par $ \c -> Put v a (c ())

-- | put a value into a @IVar@.  Multiple 'put's to the same @IVar@
-- are not allowed, and result in a runtime error.
--
-- 'put' fully evaluates its argument, which therefore must be an
-- instance of 'NFData'.  The idea is that this forces the work to
-- happen when we expect it, rather than being passed to the consumer
-- of the @IVar@ and performed later, which often results in less
-- parallelism than expected.
--
-- Sometimes partial strictness is more appropriate: see 'put_'.
--
put :: NFData a => IVar a -> a -> Par ()
put v a = deepseq a (Par $ \c -> Put v a (c ()))

-- | Allows other parallel computations to progress.  (should not be
-- necessary in most cases).
yield :: Par ()
yield = Par $ \c -> Yield (c ())
