{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP, ParallelListComp
	     #-}
{- OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind -}

-- | This module exposes the internals of the @Par@ monad so that you
-- can build your own scheduler or other extensions.  Do not use this
-- module for purposes other than extending the @Par@ monad with new
-- functionality.

module Control.Monad.Par.Scheds.TraceInternal (
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
import Data.List (partition, find)
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
  { no          :: {-# UNPACK #-} !ThreadNumber,
        -- ^ The threadnumber of this worker
    workpool    :: IORef WorkPool,
        -- ^ The workpool for this worker
    status      :: IORef AllStatus,
        -- ^ The Schedulers' status
    scheds      :: Array ThreadNumber Sched,
        -- ^ The list of all workers by thread
    tId         :: IORef ThreadId
        -- ^ The ThreadId of this worker
  }

type ThreadNumber = Int
type UId = Int
type CountRef = IORef Int
type WorkLimit = (UId, CountRef)
-- ^ The UId and the count of tasks left or Nothing if there's no limit
--   When the UId is -1, it means that the worker will remain alive until 
--   purposely killed (by globalThreadShutdown).
--
-- The reason for a work limit is to make sure that nested threads properly exit.
-- Imagine a scenario where thread A, a worker thread, encounters a runPar.  It 
-- recursively enters worker status, but it needs ot leave worker status at some 
-- point to finish the task that caused it to call runPar.  Suppose now that it 
-- encounters another call to runPar.  If it has the ability to finish and return, 
-- we must make sure it returns first for the nested runPar or else it will return 
-- to the wrong place!  The work limit helps achieve this.
--
-- TODO: Perhaps the work limit need not restrict what a thread can work on, but 
-- instead it simply provides the singular point that a thread is allowed to return 
-- from.  The only concern is some potential for bad blocking - is that a legit 
-- concern?

isWLUId :: WorkLimit -> (UId -> Bool) -> Bool
--isWLUId Nothing _ = False
isWLUId (uid, _) op = op uid

shouldEndWorkSet :: WorkLimit -> IO Bool
shouldEndWorkSet (u,_) | u == -1 = return False
shouldEndWorkSet (_, cr) = do
    c <- readIORef cr
    return (c == 0)

idleAtWL :: WorkLimit -> MVar Bool -> Idle
--idleAtWL Nothing m = Idle Nothing m
idleAtWL (uid, _) m = Idle uid m

-- | The main scheduler loop.
--   This takes the synchrony flag, our Sched, the particular work queue we're
--   currently working on, the uid of the work queue (for pushing work), our 
--   work limit, and the already-popped, first trace in the work queue.
--
--   INVARIANT: This should only be called by threads who ARE currently marked
--              as working.
sched :: Bool -> WorkLimit -> Sched -> (IORef [Trace]) -> UId -> Trace -> IO ()
sched _doSync wl q@Sched{status, workpool} queueref uid t = loop t
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
                    Empty      -> (Blocked [c],    go)
                    Full a     -> (Full a,         loop (c a))
                    Blocked cs -> (Blocked (c:cs), go)
           r
    Put (IVar v) a t  -> do
      cs <- atomicModifyIORef v $ \e -> case e of
               Empty    -> (Full a, [])
               Full _   -> error "multiple put"
               Blocked cs -> (Full a, cs)
      mapM_ (pushWork status uid queueref . ($a)) cs
      loop t
    Fork child parent -> do
         pushWork status uid queueref child
         loop parent
    Done ->
         if _doSync
         then go
                -- We could fork an extra thread here to keep numCapabilities workers
                -- even when the main thread returns to the runPar caller...
         else do -- putStrLn " [par] Forking replacement thread..\n"
                 forkIO go; return ()
                -- But even if we don't we are not orphaning any work in this
                -- thread's work-queue because it can be stolen by other threads.
                --	 else return ()

    Yield parent -> do 
        -- Go to the end of the worklist:
        -- TODO: Perhaps consider Data.Seq here.
        -- This would also be a chance to steal and work from opposite ends of the queue.
        atomicModifyIORef queueref $ \ts -> (ts++[parent],())
        go
  go = do
    mt <- atomicPopIORef queueref
    case mt of
      Just t  -> loop t
      Nothing -> do
            -- SCARY: we better be working on the top queue in the pool!
        cr <- wpRemoveWork uid workpool
        workDone <- decWorkerCount uid cr status
            -- If this uid is our workLimit id AND worker count == 0, then 
            -- we should just return () rather than calling reschedule q
        unless (isWLUId wl (== uid) && workDone) $
               reschedule wl q


-- | Process the next work queue on the work pool, or failing that, go into 
--   work stealing mode.
--
--   INVARIANT: This should only be called by threads who are NOT currently 
--              marked as working (or if they are, the task they were working 
--              on executed a runPar).
reschedule :: WorkLimit -> Sched -> IO ()
reschedule wl q@Sched{ workpool, status } = do
    wp <- readIORef workpool
    case wp of
        Work uid cr wqref _ | isWLUId wl (uid >=) -> do
            incWorkerCount cr
            nextTrace <- atomicPopIORef wqref
            case nextTrace of
                Just t  -> sched True wl q wqref uid t
                Nothing -> do
                    wpRemoveWork uid workpool
                    workDone <- decWorkerCount uid cr status
                        -- If this uid is our workLimit id AND worker count == 0, then 
                        -- we should just return () rather than calling reschedule q
                    unless (isWLUId wl (== uid) && workDone) $
                           reschedule wl q
        _ -> steal wl q


-- RRN: Note -- NOT doing random work stealing breaks the traditional
-- Cilk time/space bounds if one is running strictly nested (series
-- parallel) programs.

-- | Attempt to steal work or, failing that, give up and go idle.
steal :: WorkLimit -> Sched -> IO ()
steal wl q@Sched{ status, scheds, no=my_no } = 
  -- printf "cpu %d stealing\n" my_no >> 
  go l
  where
    (l,u) = bounds scheds
    go n
      | n > u = do
            -- Prepare to go idle
          m  <- newEmptyMVar
          atomicModifyIORef status $ addIdler (idleAtWL wl m)
            -- Check to see if this workset is ready to close
          s <- shouldEndWorkSet wl
          if s
            then do
                    -- Time to close this workset
                --printf "cpu %d shutting down workset %d\n" my_no myPriLimit
                endWorkSet status (fst wl)
                return ()
            else do
                    -- There's more work being done here, so I'll go idle
                finished <- takeMVar m
                unless finished $ go l
      | n == my_no = go (n+1)
      | otherwise  = readIORef (workpool (scheds!n)) >>= tryToSteal
          where
            tryToSteal (Work uid cr wqref wp) | isWLUId wl (uid >=) = do
                incWorkerCount cr
                stolenTrace <- atomicPopIORef wqref
                case stolenTrace of
                    Nothing -> decWorkerCount uid cr status >> tryToSteal wp
                    Just t  -> do
                        sublst <- newIORef []
                        atomicModifyIORef (workpool q) $ \wp' -> (Work uid cr sublst wp', ())
                        sched True wl q sublst uid t
            tryToSteal _ = go (n+1)


-- ---------------------------------------------------------------------------
-- UTILITY FUNCTIONS
-- ---------------------------------------------------------------------------

-- | Push work.  Then, find an idle worker with uid less than the pushed work.
-- If one is found, wake it up.
pushWork :: IORef AllStatus -> UId -> (IORef [Trace]) -> Trace -> IO ()
pushWork status uid wqref t = do
    atomicModifyIORef wqref $ (\ts -> (t:ts, ()))
    allstatus <- readIORef status
    when (hasIdleWorker uid allstatus) $ do
        r <- atomicModifyIORef status $ getIdleWorker uid
        case r of
            Just b  -> putMVar b False
            Nothing -> return ()

-- | A utility function for decreasing the task count of a work set.
-- If the count becomes 0, endWorkSet is called on the work set.
decWorkerCount :: UId -> CountRef -> IORef AllStatus -> IO Bool
decWorkerCount uid countref status = do
    done <- atomicModifyIORef countref $ 
        (\n -> if n == 0 then error "Impossible value in decWorkerCount" else (n-1, n == 1))
    when done $ (endWorkSet status uid >> globalWorkComplete uid)
    return done

-- | A utility function for increasing the task count of a work set.
incWorkerCount :: CountRef -> IO ()
incWorkerCount countref = do
    atomicModifyIORef countref $ (\n -> (n+1, ()))

-- | A utility for popping an element off of an IORef list.
-- The return value is Just a where a is the head of the list
-- or Nothing if the list is null.
atomicPopIORef :: IORef [a] -> IO (Maybe a)
atomicPopIORef ref = atomicModifyIORef ref $ \lst ->
    case lst of
        []      -> ([], Nothing)
        (e:es)  -> (es, Just e)

-- ---------------------------------------------------------------------------
-- IDLING STATUS
-- ---------------------------------------------------------------------------

data Idle    = Idle    {-# UNPACK #-} !UId (MVar Bool)
data ExtIdle = ExtIdle {-# UNPACK #-} !UId (MVar ())
type AllStatus = ([Idle], [ExtIdle])

-- | A new empty PQueue of Statuses
newStatus :: AllStatus
newStatus = ([], [])

-- | Adds a new Idler to the AllStatus.
addIdler :: Idle -> AllStatus -> (AllStatus, ())
addIdler i@(Idle u _) (is, es) = ((insert is, es), ())
    where insert [] = [i]
          insert xs@(i'@(Idle u' _):xs') = if u <= u'
            then i  : xs
            else i' : insert xs'

-- | Adds a new External idler to the AllStatus.
addExtIdler :: ExtIdle -> AllStatus -> (AllStatus, ())
addExtIdler e (is, es) = ((is, e:es), ())

-- | Returns an idle worker with uid less than or equal to the given one 
--   (if it exists) and removes it from the AllStatus
getIdleWorker :: UId -> AllStatus -> (AllStatus, Maybe (MVar Bool))
getIdleWorker u q = case q of
    ([],_) -> (q, Nothing)
    ((Idle u' m'):rst, es) -> if u' <= u then ((rst,es), Just m') else (q, Nothing)

-- | Returns true if there is an idle worker with uid less than the given one
hasIdleWorker :: UId -> AllStatus -> Bool
hasIdleWorker uid q = case getIdleWorker uid q of
    (_, Nothing) -> False
    (_, Just _)  -> True

-- | Wakes up all idle workers at the given uid with the True signal
endWorkSet :: IORef AllStatus -> UId -> IO ()
endWorkSet status uid = do
    (is, es) <- atomicModifyIORef status $ getAllAtID
    mapM_ (\(ExtIdle _ mb) -> putMVar mb ())   es
    mapM_ (\(Idle _ mb)    -> putMVar mb True) is
    where
      getAllAtID (is, es) = ((is', es'), (elems1, elems2))
        where
          (elems1, is') = partition (\(Idle    u _) -> u == uid) is
          (elems2, es') = partition (\(ExtIdle u _) -> u == uid) es


-- ---------------------------------------------------------------------------
-- WorkPool
-- ---------------------------------------------------------------------------

-- | The WorkPool keeps a queue where each element has a UId, a list of 
--   traces, and the countRef of how many workers are working on Traces 
--   of this UId.
--
--   It should be that by the natural pushing done in sched, this pool 
--   should always be in order.  We take advantage of this by making 
--   guarantees but not actually checking at runtime whether they're true.
data WorkPool = Work {-# UNPACK #-} !UId CountRef (IORef [Trace]) WorkPool | NoWork

-- | Pop the next work queue from the work pool.  This should only be called 
--   if both the work pool contains a pool, and the queue in that pool is 
--   empty.  Thus, it should only be called by the pool's owner.
wpRemoveWork :: UId -> IORef WorkPool -> IO CountRef
wpRemoveWork uid pRef = atomicModifyIORef pRef f
  where f :: WorkPool -> (WorkPool, CountRef)
        f (Work uid' cr' _ p') | uid == uid' = (p', cr')
        f (Work uid' cr' wq' p') = 
            let (p'', cr'') = f p'
            in (Work uid' cr' wq' p'', cr'')
        f NoWork = error "Impossible state in wpRemoveWork"


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

-- From outside the Par computation we can peek.  But this is
-- nondeterministic; it should perhaps have "unsafe" in the name.
pollIVar :: IVar a -> IO (Maybe a)
pollIVar (IVar ref) = 
  do contents <- readIORef ref
     case contents of 
       Full x -> return (Just x)
       _      -> return (Nothing)


-- ---------------------------------------------------------------------------
-- GLOBAL THREAD IDENTIFICATION
-- ---------------------------------------------------------------------------

-- Global thread identification is handled byt the globalThreadState object.
-- The main way to interact with this object is to attempt to establish global 
-- Scheds, shut down the threads and clear the Scheds, or to mark a work set 
-- as complete.

data GlobalThreadState = GTS (Array ThreadNumber Sched) !UId !Int

-- | This is the global thread state variable
globalThreadState :: IORef (Maybe GlobalThreadState)
globalThreadState = unsafePerformIO $ newIORef $ Nothing

-- | This is called when a work set completes (see decWorkerCount).
--   We do this so that we can know if it's okay to do a 
--   globalThreadShutdown.
globalWorkComplete :: UId -> IO ()
globalWorkComplete _ = 
    atomicModifyIORef globalThreadState f
    where f Nothing               = error "Impossible state in globalWorkComplete."
          f (Just (GTS retA n c)) = (Just (GTS retA n (c+1)), ())

-- | Attempts to set the global Scheds.  If they are already extablished, 
--   this returns a Failure with a new UId (to interact with the global 
--   threads) and the current global Scheds.  Otherwise, it establishes 
--   the given array as the global Scheds, and returns a Success containing 
--   the UId to use.
data GTSResult = Success UId | Failure UId (Array ThreadNumber Sched)
globalEstablishScheds :: Array ThreadNumber Sched -> IO GTSResult
globalEstablishScheds a = 
    atomicModifyIORef globalThreadState f
    where f Nothing               = (Just (GTS a    1     0), Success 0)
          f (Just (GTS retA n c)) = (Just (GTS retA (n+1) c), Failure n retA)

-- | Attempts to shutdown the global threads.  If there are unfinished tasks, 
--   this shuts down nothing and returns False.  Otherwise, this shuts down 
--   all threads, un-establishes the global Scheds, and returns True.
--   If the Scheds are currently unestablished, this does nothing and returns 
--   False.
--
-- TODO: This can sometimes leave threads hanging who are not doing any work 
--       but have not yet marked themselves as idle.  Things won't exactly 
--       break, but there may be MVar errors that are thrown.
globalThreadShutdown :: IO Bool
globalThreadShutdown = do 
    ma <- atomicModifyIORef globalThreadState f
    case ma of
      Nothing -> return False
      Just a  -> do
        let s = status $ a ! (fst $ bounds a)
        (is, es) <- atomicModifyIORef s $ \x -> (newStatus, x)
        mapM_ (\(ExtIdle _ m)  -> putMVar m ())    es
        mapM_ (\(Idle    _ mb) -> putMVar mb True) is
        return True
    where f (Just (GTS a n c)) | n == c = (Nothing, Just a)
          f gts = (gts, Nothing)


-- ---------------------------------------------------------------------------
-- RUNPAR
-- ---------------------------------------------------------------------------

-- [Notes on threadCapability]
--
-- We create a thread on each CPU with forkOnIO.  Ideally, the CPU on 
-- which the current thread is running will host the main thread; the 
-- other CPUs will host worker threads.
--
-- This is possible using threadCapability, but this requires
-- GHC 7.1.20110301, because that is when threadCapability was added.
--
-- Lacking threadCapability, we always pick CPU #0 to run the main
-- thread.  If the current thread is not running on CPU #0, this
-- will require some data to be shipped over the memory bus, and
-- hence will be slightly slower than the version using threadCapability.
--
-- If this is a nested runPar call, then we can do slightly better.  We 
-- can look at the current workers' ThreadIds and see if we are one of 
-- them.  If so, we do the work on that core.  If not, we are once again 
-- forced to choose arbitrarily, so we send the work to CPU #0.
--


{-# INLINE runPar_internal #-}
runPar_internal :: Bool -> Par a -> a
runPar_internal _doSync x = unsafePerformIO $ do
        -- Set up the schedulers
    myTId <- myThreadId
    tIds <- replicateM numCapabilities $ newIORef myTId
    workpools <- replicateM numCapabilities $ newIORef NoWork
    statusRef <- newIORef newStatus
    let states = listArray (0, numCapabilities-1)
                    [ Sched { no=n, workpool=wp, status=statusRef, scheds=states, tId=t }
                    | n <- [0..] | wp <- workpools | t <- tIds ]
    res <- globalEstablishScheds states
    case res of
      Success uid -> do
#if __GLASGOW_HASKELL__ >= 701 /* 20110301 */
            -- See [Notes on threadCapability] for more details
        (main_cpu, _) <- threadCapability =<< myThreadId
#else
        let main_cpu = 0
#endif
        currentWorkers <- newIORef 1
        let workLimit' = (-1, undefined)
        let workLimit = (0, currentWorkers)
        
        m <- newEmptyMVar
        rref <- newIORef Empty
        atomicModifyIORef statusRef $ addExtIdler (ExtIdle uid m)
        forM_ (elems states) $ \(state@Sched{no=cpu}) -> do
          forkOnIO cpu $ do
            myTId <- myThreadId
            --printf "cpu %d setting threadId=%s\n" cpu (show myTId)
            writeIORef (tId state) myTId
            if (cpu /= main_cpu)
              then reschedule workLimit' state
              else do
                sublst <- newIORef []
                atomicModifyIORef (workpool state) $ \wp -> (Work uid currentWorkers sublst wp, ())
                sched _doSync workLimit state sublst uid $ runCont (x >>= put_ (IVar rref)) (const Done)
        takeMVar m
        --printf "done\n"
        r <- readIORef rref
        
        -- TODO: If we're doing this nested strategy, we should probably just keep the 
        -- threads alive indefinitely.  After all, we can get some weird conditions 
        -- doing it this way.  At the least, we should put this in steal where the 
        -- shutdown occurs.
        b <- globalThreadShutdown
--         putStrLn $ "Global thread shutdown: " ++ show b
        case r of
            Full a -> return a
            _ -> error "no result"

      Failure uid cScheds -> do
#if __GLASGOW_HASKELL__ >= 701 /* 20110301 */
            -- See [Notes on threadCapability] for more details
        (main_cpu, _) <- threadCapability myTId
        cTId <- readIORef $ tId $ cScheds ! main_cpu
        let doWork = cTId == myTId
#else
        cTIds <- mapM (\s -> (readIORef $ tId $ s) >>= (\t -> return (s,t))) (elems cScheds)
        let (main_cpu, doWork) = case find ((== myTId) . snd) cTIds of
                                        Nothing    -> (0, False)
                                        Just (s,_) -> (no s, True)
#endif
        
        rref <- newIORef Empty
        let task = runCont (x >>= put_ (IVar rref)) (const Done)
            state = cScheds ! main_cpu
        if doWork
          then do
            --printf "cpu %d using old threads, of which I am one\n" main_cpu
            currentWorkers <- newIORef 1
            sublst <- newIORef []
            let workLimit = (uid, currentWorkers)
            atomicModifyIORef (workpool state) $ \wp -> (Work uid currentWorkers sublst wp, ())
            sched _doSync workLimit state sublst uid $ task
          else do
            --printf "cpu %d using old threads, of which I am not one\n" main_cpu
            currentWorkers <- newIORef 0
            sublst <- newIORef [task]
            m <- newEmptyMVar
            atomicModifyIORef (status state) $ addExtIdler (ExtIdle uid m)
            atomicModifyIORef (workpool state) $ \wp -> (Work uid currentWorkers sublst wp, ())
            takeMVar m
        --printf "cpu %d finished in child\n" main_cpu
        r <- readIORef rref
--        globalThreadShutdown
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
--   the option to "help" run the Par computation if results it is
--   interested in are not ready yet.
runParAsyncHelper :: Par a -> (a, IO ())
runParAsyncHelper = undefined -- TODO: Finish Me.


-- ---------------------------------------------------------------------------
-- PAR FUNCTIONS
-- ---------------------------------------------------------------------------

new :: Par (IVar a)
new  = Par $ New Empty

newFull :: NFData a => a -> Par (IVar a)
newFull x = deepseq x (Par $ New (Full x))

newFull_ :: a -> Par (IVar a)
newFull_ !x = Par $ New (Full x)

get :: IVar a -> Par a
get v = Par $ \c -> Get v c

put_ :: IVar a -> a -> Par ()
put_ v !a = Par $ \c -> Put v a (c ())

put :: NFData a => IVar a -> a -> Par ()
put v a = deepseq a (Par $ \c -> Put v a (c ()))

yield :: Par ()
yield = Par $ \c -> Yield (c ())
