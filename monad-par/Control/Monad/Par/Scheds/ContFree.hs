{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP, ScopedTypeVariables,
             TypeSynonymInstances, MultiParamTypeClasses,
             GeneralizedNewtypeDeriving, RecordWildCards
	     #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

{- | 

  A monad-par tranfsormer which avoids continuation passing style by
  using actual IO threads to capture continuations (but only to
  capture continuations on blocking gets).

  Note that this version makes spawn cheaper at the expense of get.
  This is largely a strawman for comparison with other schedulers.

  This implementation is similar to @Direct.hs@ without 'ContT'.
  Ideally, common pieces of the two would be factored out.

 -}


module Control.Monad.Par.Scheds.ContFree (
   Sched(..), Par, IVar(..), 
    runPar, 
    new, get, put_, fork,
    newFull, newFull_, put,
    spawn, spawn_, spawn1_, spawnP
 ) where


import Control.Applicative 
import Control.Concurrent hiding (yield)
import Control.Monad.Cont as C
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Par.Class as PC
import Control.DeepSeq
import Data.IORef
import qualified Data.Set as Set
import Debug.Trace
import GHC.IO (unsafePerformIO, unsafeDupablePerformIO)
import GHC.Conc
import System.Random.MWC as Random
import System.Mem.StableName
import Text.Printf


-- #define DEBUG
#ifdef DEBUG
dbg = True
#else
dbg = False
#endif
-- define FORKPARENT
-- define WAKEIDLE

--------------------------------------------------------------------------------
-- Core type definitions
--------------------------------------------------------------------------------

-- Our monad stack looks like this:
--      ---------
--       ReaderT
--         IO
--      ---------
-- The ReaderT monad is there for retrieving the scheduler given the
-- fact that the API calls do not get it as an argument.
-- 
newtype Par a = Par { unPar :: R.ReaderT Sched IO a }
    deriving (Monad, MonadIO, R.MonadReader Sched)

data Sched = Sched 
    { 
      ---- Per worker ----
      no       :: {-# UNPACK #-} !Int,
      workpool :: HotVar (Deque (Par ())),
      rng      :: HotVar GenIO, -- Random number gen for work stealing.
      -- Mortal set: currently distinct set per processor:
      mortal   :: HotVar (Set.Set Int), -- Could possibly use a vector....

      ---- Global data: ----
      killflag :: HotVar Bool,
      idle     :: HotVar [MVar Bool],
      scheds   :: [Sched] -- A global list of schedulers.
     }

newtype IVar a = IVar (IORef (IVarContents a))

data IVarContents a = Full a | Empty | Blocked (MVar a) 
  -- | Future a


--------------------------------------------------------------------------------
-- Helpers #1:  Simple Deques
--------------------------------------------------------------------------------

-- atomicModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
-- atomicModifyIORef (IORef (STRef r#)) f = IO $ \s -> atomicModifyMutVar# r# f s

casIORef = undefined

emptydeque :: Deque a 
addfront  :: a -> Deque a -> Deque a
addback   :: a -> Deque a -> Deque a

-- takefront :: Deque a -> Maybe (Deque a, a)
takefront :: Deque a -> (Deque a, Maybe a)
takeback  :: Deque a -> (Deque a, Maybe a)

dqlen :: Deque a -> Int

-- [2011.03.21] Presently lists are out-performing Seqs:
#if 1
newtype Deque a = DQ [a]
emptydeque = DQ []

addfront x (DQ l)    = -- trace (" * addfront brought queue up to: " ++ show (length (x:l))) $ 
		       DQ (x:l)
addback x (DQ [])    = DQ [x]
addback x (DQ (h:t)) = DQ (h : rest)
 where DQ rest = addback x (DQ t)

takefront (DQ [])     = (emptydeque, Nothing)
takefront (DQ (x:xs)) = -- trace (" * takefront popped one, remaining: " ++ show (length xs)) $ 
			(DQ xs, Just x)
takeback  (DQ [])     = (emptydeque, Nothing)
takeback  (DQ ls)     = -- trace (" * takeback popped one, remaining: " ++ show (length rest)) $ 
			(DQ rest, Just final)
 where 
  (final,rest) = loop ls []
  loop [x]    acc = (x, reverse acc)
  loop (h:tl) acc = loop tl (h:acc)
  loop []     _   = error "this shouldn't happen"
 
dqlen (DQ l) = length l
#endif

--------------------------------------------------------------------------------
-- Helpers #2:  Atomic Variables
--------------------------------------------------------------------------------
-- TEMP: Experimental

#ifndef HOTVAR
#define HOTVAR 1
#endif
newHotVar      :: a -> IO (HotVar a)
modifyHotVar   :: HotVar a -> (a -> (a,b)) -> IO b
modifyHotVar_  :: HotVar a -> (a -> a) -> IO ()
writeHotVar    :: HotVar a -> a -> IO ()
readHotVar     :: HotVar a -> IO a
-- readHotVarRaw  :: HotVar a -> m a
-- writeHotVarRaw :: HotVar a -> m a

{-# INLINE newHotVar     #-}
{-# INLINE modifyHotVar  #-}
{-# INLINE modifyHotVar_ #-}
{-# INLINE readHotVar    #-}
{-# INLINE writeHotVar   #-}

#if HOTVAR == 1
type HotVar a = IORef a
newHotVar     = newIORef
modifyHotVar  = atomicModifyIORef
modifyHotVar_ v fn = atomicModifyIORef v (\a -> (fn a, ()))
readHotVar    = readIORef
writeHotVar   = writeIORef
instance Show (IORef a) where 
  show ref = "<ioref>"

-- hotVarTransaction = id
hotVarTransaction = error "Transactions not currently possible for IO refs"
readHotVarRaw  = readHotVar
writeHotVarRaw = writeHotVar


#elif HOTVAR == 2 
#warning "Using MVars for hot atomic variables."
-- This uses MVars that are always full with *something*
type HotVar a = MVar a
newHotVar   x = do v <- newMVar; putMVar v x; return v
modifyHotVar  v fn = modifyMVar  v (return . fn)
modifyHotVar_ v fn = modifyMVar_ v (return . fn)
readHotVar    = readMVar
writeHotVar v x = do swapMVar v x; return ()
instance Show (MVar a) where 
  show ref = "<mvar>"

-- hotVarTransaction = id
-- We could in theory do this by taking the mvar to grab the lock.
-- But we'd need some temporary storage....
hotVarTransaction = error "Transactions not currently possible for MVars"
readHotVarRaw  = readHotVar
writeHotVarRaw = writeHotVar


#elif HOTVAR == 3
#warning "Using TVars for hot atomic variables."
-- Simon Marlow said he saw better scaling with TVars (surprise to me):
type HotVar a = TVar a
newHotVar = newTVarIO
modifyHotVar  tv fn = atomically (do x <- readTVar tv 
				     let (x2,b) = fn x
				     writeTVar tv x2
				     return b)
modifyHotVar_ tv fn = atomically (do x <- readTVar tv; writeTVar tv (fn x))
readHotVar x = atomically $ readTVar x
writeHotVar v x = atomically $ writeTVar v x
instance Show (TVar a) where 
  show ref = "<tvar>"

hotVarTransaction = atomically
readHotVarRaw  = readTVar
writeHotVarRaw = writeTVar

#endif

-----------------------------------------------------------------------------
-- Helpers #3:  Pushing and popping work.
-----------------------------------------------------------------------------

{-# INLINE popWork  #-}
popWork :: Sched -> IO (Maybe (Par ()))
popWork Sched{ workpool } = 
  modifyHotVar  workpool  takefront

{-# INLINE pushWork #-}
pushWork :: Sched -> Par () -> IO ()
pushWork Sched { workpool, idle } task = do
  modifyHotVar_ workpool (addfront task)
#ifdef WAKEIDLE
  -- If any worker is idle, wake one up and give it work to do.
  idles <- readHotVar idle
  when (not (null idles)) $ do
    when dbg$ printf "Waking %d idle thread(s).\n" (length idles)
    r <- modifyHotVar idle (\is -> case is of
                             [] -> ([], return ())
                             (i:is) -> (is, putMVar i False))
    r -- wake one up
#endif

rand :: HotVar GenIO -> IO Int
rand ref = uniformR (0, numCapabilities-1) =<< readHotVar ref


--------------------------------------------------------------------------------
-- Running computations in the Par monad
--------------------------------------------------------------------------------

-- instance NFData (IVar a) where
--   rnf _ = ()

runPar userComp = unsafePerformIO $ do
   allscheds <- makeScheds
  
#if __GLASGOW_HASKELL__ >= 701 /* 20110301 */
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
   let main_cpu = 0
#endif

   mv <- newEmptyMVar
   sn <- makeStableName mv

   forM_ (zip [0..] allscheds) $ \(cpu,sched) ->
        forkOnIO cpu $
          if (cpu /= main_cpu)
             then do when dbg$ printf "CPU %d entering scheduling loop.\n" cpu
		     R.runReaderT enterScheduler sched 
		     when dbg$ printf "    CPU %d exited scheduling loop.  FINISHED.\n" cpu
             else do
                  rref <- newIORef Empty
		  let userComp'  = do when dbg$ liftIO$ printf "Starting Par computation on main thread (%d).\n" main_cpu
				      res <- userComp
                                      finalSched <- R.ask 
				      when dbg$ liftIO$ printf "Out of Par computation on thread (%d).\n" (no finalSched)
#ifdef DEBUG
				      liftIO$ putStrLn$ "Result was " ++ (show res)
#endif
                                      -- If we are not purely continuation-stealing that means we may 
				      -- get to this point and have work left in our queue.  We need to reschedule.
                                      dq <- liftIO$ readHotVar (workpool finalSched)
                                      when (dqlen dq > 0) $ do
                                         when dbg$ liftIO$ putStrLn "Main thread still has work in queue... rescheduling."
					 Par enterScheduler

                                      when dbg$ liftIO$ putStrLn "MAIN THREAD ABOUT TO FORCE THE FINAL RESULT."
                                      -- After that we know that this worker has done all its work.  
				      -- However, if there's anyone else unfinished we may still 
				      -- block on "res" in the following put, which is fine.
				      put_ (IVar rref) res
		  
		  R.runReaderT (unPar userComp') sched

                  when dbg$ printf " *** Completely out of users computation.  Writing final value to MVar %d\n" (hashStableName sn)
                  readIORef rref >>= putMVar mv
   
   when dbg$ do 
      printf " *** Main thread about to take final result from MVar %d\n" (hashStableName sn)
   r <- takeMVar mv
   case r of
     Full a -> return a
     Blocked cs -> error "Work still blocked at end of Par computation. Something went wrong."
     _ -> error "No result from Par computation.  Something went wrong."


-- Create the default scheduler(s) state:
makeScheds = do
   workpools <- replicateM numCapabilities $ newHotVar emptydeque
   rngs      <- replicateM numCapabilities $ Random.create >>= newHotVar 
   idle <- newHotVar []   
   killflag <- newHotVar False
   mortals  <- mapM newHotVar (replicate numCapabilities Set.empty)
   let states = [ Sched { no=x, idle, killflag, 
			  workpool=wp, scheds=states, rng=rng,
			  mortal 
			}
                | ((x,wp),(rng,mortal)) <- zip (zip [0..] workpools) 
		                               (zip rngs mortals)]
   return states


regulatePopulation Sched{..} = 
  do 
     return ()

replaceWorker :: Sched -> IO ThreadId
replaceWorker sch@Sched{no} = do 
  -- EXPERIMENTAL: Because our deques are threadsafe on both ends we
  -- can try just reusing the same Sched structure.  
  --   We use forkOnIO to stay on the same processor as the thread we
  -- are replacing.  Therefore we expect no contention from our Sched
  -- reuse because the reusing thread is on the same physical core.
  -- 
  -- We do need to change the identifying 'no' though and thereforewe
  -- copy the Sched structure.  (We advance our 'no' by
  -- numCapabilities to avoid collision.)
  -- 
  -- TODO: Compare this partial reuse of the Sched structure to just
  -- allocating a completely fresh one. 
  -- 
  when dbg $ liftIO$ printf " [%d] Replacing with worker %d\n" no (no+numCapabilities)
  forkOnIO no (R.runReaderT enterScheduler sch{no= no + numCapabilities})

-- Register the current thread as being already replaced.
makeMortal sch@Sched{no, mortal} = 
  modifyHotVar_ mortal (Set.insert no)
  

--------------------------------------------------------------------------------
-- IVar operations
--------------------------------------------------------------------------------

{-# INLINE new  #-}
-- | creates a new @IVar@
new :: Par (IVar a)
new  = liftIO $ do r <- newIORef Empty
                   return (IVar r)

-- This makes debugging somewhat easier:
spinReadMVar :: Sched -> MVar b -> IO b
spinReadMVar Sched{..} mv = do
  sn <- makeStableName mv
  let loop = do v <- tryTakeMVar mv
		case v of 
		  Nothing -> do printf "                 [%d] thread spinning to try to read MVar %d\n" no (hashStableName sn) 
				threadDelay (100 * 1000) -- 1/10 a second
				loop 
		  Just x -> do printf "[%d] SUCCESSFULLY read MVar %d\n" no (hashStableName sn) 
			       putMVar mv x
			       return x
  loop
		  

{-# INLINE get  #-}
-- | read the value in a @IVar@.  The 'get' can only return when the
-- value has been written by a prior or parallel @put@ to the same
-- @IVar@.
get (IVar v) = do 
       -- Optimistically read without writing, maybe the value is already there:
       e  <- liftIO$ readIORef v
       case e of
	  Full a   -> return a
--	  Future a -> return a
          -- Otherwise we need to do a write:
	  _ -> do
            sch <- R.ask
            mv <- liftIO$ newEmptyMVar
	    sn <- liftIO$ makeStableName mv
            let rd mv x = 
                   let 
#ifdef DEBUG
		       act = spinReadMVar sch mv
#else
		       act = readMVar mv
#endif
		       act' = -- do regulatePopulation sch; act
		              do makeMortal sch; replaceWorker sch; act
		       val = unsafePerformIO act' -- dupable?
		   in (x, val)

            when dbg$ liftIO$ printf "  It looks like we may go down to block MVar %d for a get.  Thread %d\n" (hashStableName sn) (no sch)

	    -- The thread modifying the IO ref will block until the MVar is filled in:
	    val <- liftIO$ atomicModifyIORef v $ \e -> 
	     case e of
	       Empty         -> rd mv (Blocked mv)
	       x@(Full a)    -> (x, a) -- This happened 1114 times on fib(24)
	       x@(Blocked mv) -> rd mv x 
            -- Make sure that we have the actual value (not just a thunk) before proceding:
#ifdef DEBUG
            trace (" !! performing unsafeIO read of MVar "++ show (hashStableName sn) ++"... thread "++ show (no sch)) $
             val `pseq` 
              trace (" !! COMPLETED read of MVar "++ show (hashStableName sn) ++" on thread "++ show (no sch) ++ " value " ++ show val) $
              (return val)
#else
            val `pseq` return val
#endif



{-# INLINE put_ #-}
-- | @put_@ is a version of @put@ that is head-strict rather than fully-strict.
put_ (IVar v) !content = do
   sched <- R.ask -- Have to do this here because the modify fn is pure:
#ifdef DEBUG
   sn <- liftIO$ makeStableName v
   when dbg$ liftIO$ printf "    !! [%d] PUTTING value %s to ivar %d\n" (no sched) (show content) (hashStableName sn)
#endif
   liftIO$ do 
      mmv <- atomicModifyIORef v $ \e -> case e of
               Empty      -> (Full content, Nothing)
               Blocked mv -> (Full content, Just mv)
               Full _     -> error "multiple put"
      case mmv of 
        Just mv -> do 
		      when dbg$ do sn <- makeStableName mv
				   printf "    !! [thread %d] Putting MVar %d, unblocking thread(s).\n"  (hashStableName sn) (no sched)
		      putMVar mv content
        Nothing -> return () 

{-# INLINE fork #-}
fork :: Par () -> Par ()
fork task = do
   sch <- R.ask
   liftIO$ when dbg$ printf "  *** forking task from cpu %d...\n" (no sch)
   liftIO$ pushWork sch task

-- enterScheduler is the main entrypoint for the scheduler.
enterScheduler :: R.ReaderT Sched IO ()
enterScheduler = do
  mysched <- R.ask 
  when dbg$ liftIO$ printf " - Reschedule: CPU %d\n" (no mysched)
  mtask <- liftIO$ popWork mysched
  case mtask of
    Nothing -> liftIO (steal mysched)
    Just task -> do
       -- When popping work from our own queue the Sched (Reader value) stays the same:
       when dbg $ liftIO$ printf "  popped work from own queue (cpu %d)\n" (no mysched)
       -- Run the stolen tasK:
       unPar task 
       when dbg$ do sch <- R.ask; liftIO$ printf "  + task finished successfully on cpu %d\n" (no sch)
       -- Before going around again, we check if we have become "mortal": 
       mortset <- liftIO$ readHotVar (mortal mysched)
       if Set.member (no mysched) mortset then
          when dbg$ liftIO$ printf " [%d] Thread is mortal, shutting down.\n" (no mysched)
        else enterScheduler 
   
-- | Attempt to steal work or, failing that, give up and go idle.
steal :: Sched -> IO ()
steal mysched@Sched{ idle, scheds, rng, no=my_no } = do 
  when dbg$ printf "cpu %d stealing\n" my_no
  i <- rand rng  -- Pick an initial victim.
  go maxtries i
 where
#ifdef WAKEIDLE
    maxtries = numCapabilities -- How many times should we attempt theft before going idle?
#else
    maxtries = 20 * numCapabilities -- More if they don't wake up after.
#endif

    ----------------------------------------
    -- IDLING behavior:
    go 0 _ = 
            do mv <- newEmptyMVar
               when dbg$ printf "cpu %d Tired of stealing... giving up.\n" my_no
               r <- modifyHotVar idle $ \is -> (mv:is, is)
               if length r == numCapabilities - 1
                  then do
                     -- If we were the LAST one to go idle:
                     when dbg$ printf "cpu %d initiating shutdown\n" my_no
                     mapM_ (\m -> putMVar m True) r
                  else do
                    when dbg$ printf "cpu %d taking mvar to go idle...\n" my_no
                    done <- takeMVar mv
                    if done
                       then do
                         when dbg$ printf "cpu %d shutting down\n" my_no
                         return ()
                       else do
                         when dbg$ printf "cpu %d woken up\n" my_no
			 i <- rand rng
                         go maxtries i

    ----------------------------------------
    go tries i
      | i == my_no = do i' <- rand rng
			go (tries-1) i'

      | otherwise     = do
         let schd = scheds!!i -- TEMP, FIXME, linear access of a list. 
         when dbg$ printf "cpu %d trying steal from %d\n" my_no (no schd)

         r <- modifyHotVar (workpool schd) takeback
         case r of
           Just task  -> do
              when dbg$ printf "cpu %d got work from cpu %d\n" my_no (no schd)
	      R.runReaderT (unPar task) mysched 
           Nothing -> do i' <- rand rng 
			 go (tries-1) i'

{-# INLINE spawn1_ #-}
-- Spawn a one argument function instead of a thunk.  This is good for debugging if the value supports "Show".
spawn1_ f x = 
#ifdef DEBUG
 do sch <- R.ask; when dbg$ liftIO$ printf " [%d] spawning fn with arg %s\n" (no sch) (show x)
#endif
    spawn_ (f x)


----------------------------------------------------------------------------------------------------
-- TEMPORARY -- SCRAP:

-- The following is usually inefficient! 
newFull_ a = do v <- new
		put_ v a
		return v

newFull :: NFData a => a -> Par (IVar a)
newFull a = deepseq a (newFull a)

{-# INLINE put #-}
put v a = deepseq a (put_ v a)

-- Here we use the same mechanism an in Spark.hs:
spawnP :: NFData a => a -> Par (IVar a)
spawnP a = do ref <- liftIO$ newIORef (Full (rnf a `pseq` a))
	      a `par` return (IVar ref)



--------------------------------------------------------------------------------
-- MonadPar instance for IO; TEMPORARY
--------------------------------------------------------------------------------

-- <boilerplate>
spawn p  = do r <- new;  fork (p >>= put r);   return r
spawn_ p = do r <- new;  fork (p >>= put_ r);  return r
-- </boilerplate>

#ifdef DEBUG
put    :: (Show a, NFData a) => IVar a -> a -> Par ()
spawn  :: (Show a, NFData a) => Par a -> Par (IVar a)
spawn_ :: Show a => Par a -> Par (IVar a)
put_   :: Show a => IVar a -> a -> Par ()
get    :: Show a => IVar a -> Par a
runPar :: Show a => Par a -> a 
newFull_ ::  Show a => a -> Par (IVar a)
#else
spawn  :: NFData a => Par a -> Par (IVar a)
spawn_ :: Par a -> Par (IVar a)
put_   :: IVar a -> a -> Par ()
put    :: NFData a => IVar a -> a -> Par ()
get    :: IVar a -> Par a
runPar :: Par a -> a 
newFull_ ::  a -> Par (IVar a)

instance PC.ParFuture IVar Par where
  get    = get
  spawn  = spawn
  spawn_ = spawn_
  spawnP = spawnP

instance PC.ParIVar IVar Par where
  fork = fork
  new  = new
  put_ = put_
  newFull = newFull
  newFull_ = newFull_
#endif

instance Functor Par where
   fmap f xs = xs >>= return . f

instance Applicative Par where
   (<*>) = ap
   pure  = return
