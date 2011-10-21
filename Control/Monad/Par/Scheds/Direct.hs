{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP, ScopedTypeVariables,
             TypeSynonymInstances, MultiParamTypeClasses,
             GeneralizedNewtypeDeriving
	     #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- | A scheduler for the Par monad based on directly performing IO
-- actions when Par methods are called (i.e. without using a lazy
-- trace data structure).

module Control.Monad.Par.Scheds.Direct (
   Sched(..), Par,
   IVar(..), IVarContents(..),
--    sched,
    runPar, 
    new, get, put_, fork,
    newFull, newFull_, put,
    spawn, spawn_, spawnP 
--   runParAsync, runParAsyncHelper,
--   pollIVar, yield,
 ) where

import Control.Applicative
import Control.Concurrent hiding (yield)
import Debug.Trace
import Data.IORef
import Text.Printf
import GHC.Conc
import Control.Monad.Cont as C
import qualified Control.Monad.Reader as R
-- import qualified Data.Array as A
-- import qualified Data.Vector as A
import qualified Data.Sequence as Seq
import System.Random as Random
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Monad.Par.Class as PC
import Control.DeepSeq

--------------------------------------------------------------------------------
-- Configuration Toggles
--------------------------------------------------------------------------------

dbg = False
-- define FORKPARENT
-- define WAKEIDLE

--------------------------------------------------------------------------------
-- Core type definitions
--------------------------------------------------------------------------------

-- Our monad stack looks like this:
--      ---------
--        ContT
--       ReaderT
--         IO
--      ---------
-- The ReaderT monad is there for retrieving the scheduler given the
-- fact that the API calls do not get it as an argument.
-- 
-- Note that the result type for continuations is unit.  Forked
-- computations return nothing.
--
newtype Par a = Par { unPar :: C.ContT () ROnly a }
    deriving (Monad, MonadIO, MonadCont, R.MonadReader Sched)
type ROnly = R.ReaderT Sched IO

data Sched = Sched 
    { 
      no       :: {-# UNPACK #-} !Int,
      workpool :: HotVar (Deque (Par ())),
      killflag :: HotVar Bool,
      idle     :: HotVar [MVar Bool],
      rng      :: HotVar StdGen, -- Random number gen for work stealing.
      scheds   :: [Sched] -- A global list of schedulers.
     }

newtype IVar a = IVar (IORef (IVarContents a))

data IVarContents a = Full a | Empty | Blocked [a -> Par ()]


--------------------------------------------------------------------------------
-- Helpers #1:  Simple Deques
--------------------------------------------------------------------------------

--atomicModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
--atomicModifyIORef (IORef (STRef r#)) f = IO $ \s -> atomicModifyMutVar# r# f s

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

rand :: HotVar StdGen -> IO Int
rand ref = 
 do g <- readHotVar ref
    let (n,g') = next g
	i = n `mod` numCapabilities
    writeHotVar ref g'
    return i


--------------------------------------------------------------------------------
-- Running computations in the Par monad
--------------------------------------------------------------------------------

-- instance NFData (IVar a) where
--   rnf _ = ()

runPar :: Par a -> a 
runPar userComp = unsafePerformIO $ do
   states <- makeScheds
  
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

   m <- newEmptyMVar
   forM_ (zip [0..] states) $ \(cpu,state) ->
        forkOnIO cpu $
          if (cpu /= main_cpu)
             then do when dbg$ printf "CPU %d entering scheduling loop.\n" cpu
		     runReaderWith state $ rescheduleR unused
		     when dbg$ printf "    CPU %d exited scheduling loop.  FINISHED.\n" cpu
             else do
                  rref <- newIORef Empty
		  let userComp'  = do when dbg$ liftIO$ printf "Starting Par computation on main thread (%d).\n" main_cpu
				      res <- userComp
                                      finalSched <- R.ask 
				      when dbg$ liftIO$ printf "Out of Par computation on thread (%d).\n" (no finalSched)

				      -- Sanity check our work queues:
				      when dbg $ liftIO$ do
					forM_ states $ \ Sched{no, workpool} -> do
					  dq <- readHotVar workpool
					  when (dqlen dq > 0) $ do 
					     printf "WARNING: After main thread exited non-empty queue remains for worker %d\n" no
					printf "Sanity check complete.\n"

				      put_ (IVar rref) res

		  
		  R.runReaderT (C.runContT (unPar userComp') trivialCont) state

                  when dbg$ putStrLn " *** Completely out of users computation.  Writing final value."
                  readIORef rref >>= putMVar m
   r <- takeMVar m
   case r of
     Full a -> return a
     Blocked cs -> error "Work still blocked at end of Par computation. Something went wrong."
     _ -> error "No result from Par computation.  Something went wrong."


-- Create the default scheduler(s) state:
makeScheds = do
   workpools <- replicateM numCapabilities $ newHotVar emptydeque
   rngs      <- replicateM numCapabilities $ newStdGen >>= newHotVar 
   idle <- newHotVar []   
   killflag <- newHotVar False
   let states = [ Sched { no=x, idle, killflag, 
			  workpool=wp, scheds=states, rng=rng
			}
                | (x,wp,rng) <- zip3 [0..] workpools rngs]
   return states



--------------------------------------------------------------------------------
-- IVar operations
--------------------------------------------------------------------------------

{-# INLINE new  #-}
-- | creates a new @IVar@
new :: Par (IVar a)
new  = liftIO $ do r <- newIORef Empty
                   return (IVar r)

{-# INLINE get  #-}
-- | read the value in a @IVar@.  The 'get' can only return when the
-- value has been written by a prior or parallel @put@ to the same
-- @IVar@.
get :: IVar a -> Par a
get (IVar v) =  do 
  callCC $ \cont -> 
    do
       e  <- liftIO$ readIORef v
       case e of
	  Full a -> return a
	  _ -> do
#ifndef FORKPARENT
            sch <- R.ask
            let resched = -- trace (" cpu "++ show (no sch) ++ " rescheduling on unavailable ivar!") 
			  reschedule
            -- Because we continue on the same processor the Sched stays the same:
            -- TODO: Try NOT using monads as first class values here.  Check for performance effect:
	    r <- liftIO$ atomicModifyIORef v $ \e -> case e of
		      Empty      -> (Blocked [cont], resched)
		      Full a     -> (Full a, return a)
		      Blocked cs -> (Blocked (cont:cs), resched)
	    r
#else
--------------------------------------------------
-- TEMP: Debugging: Here's a spinning version:
--------------------------------------------------
-- Only works for most programs with parent/continuation stealing:
            let loop = do snap <- readIORef v
			  case snap of 
			    Empty      -> do putStr "."; loop 
			    Full a     -> return a
            liftIO loop
#endif

{-# INLINE put_ #-}
-- | @put_@ is a version of @put@ that is head-strict rather than fully-strict.
put_ :: IVar a -> a -> Par ()
put_ (IVar v) !content = do
   sched <- R.ask 
   liftIO$ do 
      cs <- atomicModifyIORef v $ \e -> case e of
               Empty    -> (Full content, [])
               Full _   -> error "multiple put"
               Blocked cs -> (Full content, cs)
      mapM_ (pushWork sched . ($content)) cs
      return ()

-- TODO: Continuation (parent) stealing version.
{-# INLINE fork #-}
fork :: Par () -> Par ()
#ifdef FORKPARENT
#warning "FORK PARENT POLICY USED"
fork task = do 
   sched <- R.ask   
   callCC$ \parent -> do
      let wrapped = parent ()
      -- Is it possible to slip in a new Sched here?
      -- let wrapped = lift$ R.runReaderT (parent ()) undefined
      liftIO$ pushWork sched wrapped
      -- Then execute the child task and return to the scheduler when it is complete:
      task 
      -- If we get to this point we have finished the child task:
      reschedule
      liftIO$ putStrLn " !!! ERROR: Should not reach this point #1"   

-- TODO!! The Reader monad may need to become a State monad.  When a
-- continuation is stolen it has moved to a new Sched and needs to be
-- updated.
   
   when dbg$ do 
    sched2 <- R.ask 
    liftIO$ printf "     called parent continuation... was on cpu %d now on cpu %d\n" (no sched) (no sched2)

#else
fork task = do
   sch <- R.ask
   liftIO$ when dbg$ printf "  forking task from cpu %d...\n" (no sch)
   liftIO$ pushWork sch task
#endif
   
-- This routine "longjmp"s to the scheduler, throwing out its own continuation.
reschedule :: Par a 
reschedule = Par $ C.ContT rescheduleR

-- Reschedule ignores its continuation:
rescheduleR :: ignoredCont -> ROnly ()
rescheduleR k = do
  mysched <- R.ask 
  when dbg$ liftIO$ printf " - Reschedule: CPU %d\n" (no mysched)
  mtask <- liftIO$ popWork mysched
  case mtask of
    Nothing -> liftIO (steal mysched)
    Just task -> do
       -- When popping work from our own queue the Sched (Reader value) stays the same:
       when dbg $ liftIO$ printf "  popped work from own queue (cpu %d)\n" (no mysched)
       let C.ContT fn = unPar task 
       -- Run the stolen task with a continuation that returns to the scheduler if the task exits normally:
       fn (\ () -> do 
           sch <- R.ask
           when dbg$ liftIO$ printf "  + task finished successfully on cpu %d, calling reschedule continuation..\n" (no sch)
	   rescheduleR (error "unused continuation"))


{-# INLINE runReaderWith #-}
runReaderWith state m = R.runReaderT m state


-- | Attempt to steal work or, failing that, give up and go idle.
steal :: Sched -> IO ()
steal mysched@Sched{ idle, scheds, rng, no=my_no } = do
  when dbg$ printf "cpu %d stealing\n" my_no
  i <- getnext (-1 :: Int)
  go maxtries i
 where
#ifdef WAKEIDLE
    maxtries = numCapabilities -- How many times should we attempt theft before going idle?
#else
    maxtries = 20 * numCapabilities -- More if they don't wake up after.
#endif

    getnext _ = rand rng

    ----------------------------------------
    -- IDLING behavior:
    go 0 _ = 
            do m <- newEmptyMVar
               r <- modifyHotVar idle $ \is -> (m:is, is)
               if length r == numCapabilities - 1
                  then do
                     when dbg$ printf "cpu %d initiating shutdown\n" my_no
                     mapM_ (\m -> putMVar m True) r
                  else do
                    done <- takeMVar m
                    if done
                       then do
                         when dbg$ printf "cpu %d shutting down\n" my_no
                         return ()
                       else do
                         when dbg$ printf "cpu %d woken up\n" my_no
			 i <- getnext (-1::Int)
                         go maxtries i
    ----------------------------------------
    go tries i
      | i == my_no = do i' <- getnext i
			go (tries-1) i'

      | otherwise     = do
         let schd = scheds!!i
         when dbg$ printf "cpu %d trying steal from %d\n" my_no (no schd)

         r <- modifyHotVar (workpool schd) takeback

         case r of
           Just task  -> do
              when dbg$ printf "cpu %d got work from cpu %d\n" my_no (no schd)
	      runReaderWith mysched $ 
		C.runContT (unPar task)
		 (\_ -> do
		   when dbg$ liftIO$ printf "cpu %d DONE running stolen work from %d\n" my_no (no schd)
		   return ())

           Nothing -> do i' <- getnext i
			 go (tries-1) i'

unused = error "this closure shouldn't be used"
trivialCont _ = trace "trivialCont evaluated!"
		return ()

----------------------------------------------------------------------------------------------------
-- TEMP: Factor out this boilerplate somehow.

-- <boilerplate>
newFull_ ::  a -> Par (IVar a)
-- The following is usually inefficient! 
newFull_ a = do v <- new
		put_ v a
		return v

newFull :: NFData a => a -> Par (IVar a)
newFull a = deepseq a (newFull_ a)

{-# INLINE put  #-}
put :: NFData a => IVar a -> a -> Par ()
put v a = deepseq a (put_ v a)
-- </boilerplate>

--------------------------------------------------------------------------------
-- <boilerplate>
spawn  :: NFData a => Par a -> Par (IVar a)
spawn_ :: Par a -> Par (IVar a)
spawnP :: NFData a => a -> Par (IVar a)

spawn p  = do r <- new;  fork (p >>= put r);   return r
spawn_ p = do r <- new;  fork (p >>= put_ r);  return r
spawnP a = spawn (return a)

instance PC.ParFuture Par IVar where
  get    = get
  spawn  = spawn
  spawn_ = spawn_
  spawnP = spawnP

instance PC.ParIVar Par IVar where
  fork = fork
  new  = new
  put_ = put_
  newFull = newFull
  newFull_ = newFull_

instance Functor Par where
   fmap f xs = xs >>= return . f

instance Applicative Par where
   (<*>) = ap
   pure  = return
-- </boilerplate>
--------------------------------------------------------------------------------
