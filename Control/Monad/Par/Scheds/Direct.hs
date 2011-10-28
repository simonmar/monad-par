{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP, ScopedTypeVariables,
             TypeSynonymInstances, MultiParamTypeClasses,
             GeneralizedNewtypeDeriving, PackageImports
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
    spawn, spawn_, spawnP,
    spawn1_
--   runParAsync, runParAsyncHelper,
--   pollIVar, yield,
 ) where

import Control.Applicative
import Control.Concurrent hiding (yield)
import Debug.Trace
import Data.IORef
import Text.Printf
import GHC.Conc
import "mtl" Control.Monad.Cont as C
import qualified "mtl" Control.Monad.Reader as R
-- import qualified Data.Array as A
-- import qualified Data.Vector as A
import qualified Data.Sequence as Seq
import System.Random as Random
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName
import qualified Control.Monad.Par.Class as PC
import Control.DeepSeq

--------------------------------------------------------------------------------
-- Configuration Toggles
--------------------------------------------------------------------------------

-- define DEBUG
#ifdef DEBUG
dbg = True
#else
dbg = False
#endif

#define FORKPARENT
#define WAKEIDLE

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
      ---- Per worker ----
      no       :: {-# UNPACK #-} !Int,
      workpool :: HotVar (Deque (Par ())),
      rng      :: HotVar StdGen, -- Random number gen for work stealing.
      isMain :: Bool, -- Are we the main/master thread? 

      ---- Global data: ----
      killflag :: HotVar Bool,
      idle     :: HotVar [MVar Bool],
      scheds   :: [Sched]        -- A global list of schedulers.
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

addfront x (DQ l)    = 
#ifdef DEBUG
                       trace ("                                        * addfront |Q| = " ++ show (length (x:l))) $ 
#endif
		       DQ (x:l)
addback x (DQ [])    = DQ [x]
addback x (DQ (h:t)) = DQ (h : rest)
 where DQ rest = addback x (DQ t)

takefront (DQ [])     = (emptydeque, Nothing)
takefront (DQ (x:xs)) = 
#ifdef DEBUG
                        trace ("                                        * takefront |Q| = " ++ show (length xs)) $ 
#endif
			(DQ xs, Just x)

-- EXPENSIVE:
takeback  (DQ [])     = (emptydeque, Nothing)
takeback  (DQ ls)     = 
#ifdef DEBUG
                        trace ("                                        * takeback |Q| = " ++ show (length rest)) $ 
#endif
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
popWork Sched{ workpool, no } = do 
  mb <- modifyHotVar  workpool  takefront
  if dbg 
   then case mb of 
         Nothing -> return Nothing
	 Just x  -> do sn <- makeStableName mb
		       printf " [%d]                                   -> POP work unit %d\n" no (hashStableName sn)
		       return mb
   else return mb

{-# INLINE pushWork #-}
pushWork :: Sched -> Par () -> IO ()
pushWork Sched { workpool, idle, no, isMain } task = do
  modifyHotVar_ workpool (addfront task)
  when dbg $ do sn <- makeStableName task
		printf " [%d]                                   -> PUSH work unit %d\n" no (hashStableName sn)
#ifdef WAKEIDLE
  --when isMain$ 
  tryWakeIdle idle
#endif


tryWakeIdle idle = do
-- NOTE: I worry about having the idle var hammmered by all threads on their spawn-path:
  -- If any worker is idle, wake one up and give it work to do.
  idles <- readHotVar idle -- Optimistically do a normal read first.
  when (not (null idles)) $ do
    when dbg$ printf "Waking %d idle thread(s).\n" (length idles)
    r <- modifyHotVar idle (\is -> case is of
                             []     -> ([], return ())
                             (i:is) -> (is, putMVar i False))
    r -- wake an idle worker up by putting an MVar.

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

runPar userComp = unsafePerformIO $ do
  
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
   allscheds <- makeScheds main_cpu

   m <- newEmptyMVar
   forM_ (zip [0..] allscheds) $ \(cpu,sched) ->
        forkOnIO cpu $
          if (cpu /= main_cpu)
             then do when dbg$ printf " [%d] Entering scheduling loop.\n" cpu
		     runReaderWith sched $ rescheduleR errK
		     when dbg$ printf " [%d] Exited scheduling loop.  FINISHED.\n" cpu
             else do
		  let userComp'  = do when dbg$ liftIO$ printf " [%d] Starting Par computation on main thread.\n" main_cpu
				      res <- userComp
                                      finalSched <- R.ask 
				      when dbg$ liftIO$ printf " [%d] Out of Par computation on main thread.  Writing MVar...\n" (no finalSched)

				      -- Sanity check our work queues:
				      when dbg $ sanityCheck allscheds
				      liftIO$ putMVar m res
		  
		  R.runReaderT (C.runContT (unPar userComp') trivialCont) sched
                  when dbg$ do putStrLn " *** Out of entire runContT user computation on main thread."
                               sanityCheck allscheds
		  -- Not currently requiring that other scheduler threads have exited before we 
		  -- (the main thread) exit.  But we do signal here that they should terminate:
                  writeIORef (killflag sched) True

   when dbg$ do putStrLn " *** Reading final MVar on main thread."
   takeMVar m -- Final value.


-- Make sure there is no work left in any deque after exiting.
sanityCheck allscheds = liftIO$ do
  forM_ allscheds $ \ Sched{no, workpool} -> do
     dq <- readHotVar workpool
     when (dqlen dq > 0) $ do 
         printf "WARNING: After main thread exited non-empty queue remains for worker %d\n" no
  liftIO$ putStrLn "Sanity check complete."


-- Create the default scheduler(s) state:
makeScheds main = do
   workpools <- replicateM numCapabilities $ newHotVar emptydeque
   rngs      <- replicateM numCapabilities $ newStdGen >>= newHotVar 
   idle <- newHotVar []   
   killflag <- newHotVar False
   let allscheds = [ Sched { no=x, idle, killflag, isMain= (x==main),
			     workpool=wp, scheds=allscheds, rng=rng
			}
                | (x,wp,rng) <- zip3 [0..] workpools rngs]
   return allscheds



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
get iv@(IVar v) =  do 
  callCC $ \cont -> 
    do
       e  <- liftIO$ readIORef v
       case e of
	  Full a -> return a
	  _ -> do
            sch <- R.ask
#  ifdef DEBUG
            sn <- liftIO$ makeStableName iv
            let resched = trace (" ["++ show (no sch) ++ "]  - Rescheduling on unavailable ivar "++show (hashStableName sn)++"!") 
#else
            let resched = 
#  endif
			  reschedule
            -- Because we continue on the same processor the Sched stays the same:
            -- TODO: Try NOT using monads as first class values here.  Check for performance effect:
	    r <- liftIO$ atomicModifyIORef v $ \e -> case e of
		      Empty      -> (Blocked [cont], resched)
		      Full a     -> (Full a, return a)
		      Blocked cs -> (Blocked (cont:cs), resched)
	    r

{-# INLINE put_ #-}
-- | @put_@ is a version of @put@ that is head-strict rather than fully-strict.
put_ iv@(IVar v) !content = do
   sched <- R.ask 
   liftIO$ do 
      cs <- atomicModifyIORef v $ \e -> case e of
               Empty    -> (Full content, [])
               Full _   -> error "multiple put"
               Blocked cs -> (Full content, cs)

#ifdef DEBUG
      sn <- liftIO$ makeStableName iv
      printf " [%d] Put value %s into IVar %d.  Waking up %d continuations.\n" 
	     (no sched) (show content) (hashStableName sn) (length cs)
#endif
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
      reschedule -- We reschedule to pop the cont we pushed.
      liftIO$ putStrLn " !!! ERROR: Should not reach this point #1"   

   when dbg$ do 
    sched2 <- R.ask 
    liftIO$ printf "     called parent continuation... was on cpu %d now on cpu %d\n" (no sched) (no sched2)

#else
fork task = do
   sch <- R.ask
   liftIO$ when dbg$ printf " [%d] forking task...\n" (no sch)
   liftIO$ pushWork sch task
#endif
   
-- This routine "longjmp"s to the scheduler, throwing out its own continuation.
reschedule :: Par a 
reschedule = Par $ C.ContT rescheduleR

-- Reschedule ignores its continuation.
-- It runs the scheduler loop indefinitely, until it observers killflag==True
rescheduleR :: ignoredCont -> ROnly ()
rescheduleR k = do
  mysched <- R.ask 
  when dbg$ liftIO$ printf " [%d]  - Reschedule...\n" (no mysched)
  mtask <- liftIO$ popWork mysched
  case mtask of
    Nothing -> do k <- liftIO$ readIORef (killflag mysched) 
		  unless k $ do		    
		     liftIO (steal mysched)
#ifdef WAKEIDLE
--                     liftIO$ tryWakeIdle (idle mysched)
#endif
		     rescheduleR errK
    Just task -> do
       -- When popping work from our own queue the Sched (Reader value) stays the same:
       when dbg $ do sn <- liftIO$ makeStableName task
		     liftIO$ printf " [%d] popped work %d from own queue\n" (no mysched) (hashStableName sn)
       let C.ContT fn = unPar task 
       -- Run the stolen task with a continuation that returns to the scheduler if the task exits normally:
       fn (\ () -> do 
           sch <- R.ask
           when dbg$ liftIO$ printf "  + task finished successfully on cpu %d, calling reschedule continuation..\n" (no sch)
	   rescheduleR errK)

{-# INLINE runReaderWith #-}
runReaderWith state m = R.runReaderT m state


-- | Attempt to steal work or, failing that, give up and go idle.
steal :: Sched -> IO ()
steal mysched@Sched{ idle, scheds, rng, no=my_no } = do
  when dbg$ printf " [%d]  + stealing\n" my_no
  i <- getnext (-1 :: Int)
  go maxtries i
 where
--    maxtries = numCapabilities -- How many times should we attempt theft before going idle?
    maxtries = 20 * numCapabilities -- How many times should we attempt theft before going idle?

    getnext _ = rand rng

    ----------------------------------------
    -- IDLING behavior:
    go 0 _ = 
            do m <- newEmptyMVar
               r <- modifyHotVar idle $ \is -> (m:is, is)
               if length r == numCapabilities - 1
                  then do
                     when dbg$ printf " [%d]  | initiating shutdown\n" my_no
                     mapM_ (\m -> putMVar m True) r
                  else do
                    done <- takeMVar m
                    if done
                       then do
                         when dbg$ printf " [%d]  | shutting down\n" my_no
                         return ()
                       else do
                         when dbg$ printf " [%d]  | woken up\n" my_no
			 i <- getnext (-1::Int)
                         go maxtries i
    ----------------------------------------
    go tries i
      | i == my_no = do i' <- getnext i
			go (tries-1) i'

      | otherwise     = do
         let schd = scheds!!i
         when dbg$ printf " [%d]  | trying steal from %d\n" my_no (no schd)

         r <- modifyHotVar (workpool schd) takeback

         case r of
           Just task  -> do
              when dbg$ do sn <- liftIO$ makeStableName task
			   printf " [%d]  | stole work (unit %d) from cpu %d\n" my_no (hashStableName sn) (no schd)
	      runReaderWith mysched $ 
		C.runContT (unPar task)
		 (\_ -> do
		   when dbg$ do sn <- liftIO$ makeStableName task
		                liftIO$ printf " [%d]  | DONE running stolen work (unit %d) from %d\n" my_no (hashStableName sn) (no schd)
		   return ())

           Nothing -> do i' <- getnext i
			 go (tries-1) i'

errK = error "this closure shouldn't be used"
trivialCont _ = 
#ifdef DEBUG
                trace "trivialCont evaluated!"
#endif
		return ()

----------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------
-- <boilerplate>

-- TEMP: TODO: Factor out this boilerplate somehow.

{-# INLINE spawn1_ #-}
-- Spawn a one argument function instead of a thunk.  This is good for debugging if the value supports "Show".
spawn1_ f x = 
#ifdef DEBUG
 do sn  <- liftIO$ makeStableName f
    sch <- R.ask; when dbg$ liftIO$ printf " [%d] spawning fn %d with arg %s\n" (no sch) (hashStableName sn) (show x)
#endif
    spawn_ (f x)

-- The following is usually inefficient! 
newFull_ a = do v <- new
		put_ v a
		return v

newFull a = deepseq a (newFull_ a)

{-# INLINE put  #-}
put v a = deepseq a (put_ v a)

spawn p  = do r <- new;  fork (p >>= put r);   return r
spawn_ p = do r <- new;  fork (p >>= put_ r);  return r
spawnP a = spawn (return a)

-- In Debug mode we require that IVar contents be Show-able:
#ifdef DEBUG
put    :: (Show a, NFData a) => IVar a -> a -> Par ()
spawn  :: (Show a, NFData a) => Par a -> Par (IVar a)
spawn_ :: Show a => Par a -> Par (IVar a)
spawn1_ :: (Show a, Show b) => (a -> Par b) -> a -> Par (IVar b)
put_   :: Show a => IVar a -> a -> Par ()
get    :: Show a => IVar a -> Par a
runPar :: Show a => Par a -> a 
newFull :: (Show a, NFData a) => a -> Par (IVar a)
newFull_ ::  Show a => a -> Par (IVar a)
#else
spawn  :: NFData a => Par a -> Par (IVar a)
spawn_ :: Par a -> Par (IVar a)
spawn1_ :: (a -> Par b) -> a -> Par (IVar b)
put_   :: IVar a -> a -> Par ()
put    :: NFData a => IVar a -> a -> Par ()
get    :: IVar a -> Par a
runPar :: Par a -> a 
newFull :: NFData a => a -> Par (IVar a)
newFull_ ::  a -> Par (IVar a)


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
#endif

instance Functor Par where
   fmap f xs = xs >>= return . f

instance Applicative Par where
   (<*>) = ap
   pure  = return
-- </boilerplate>
--------------------------------------------------------------------------------
