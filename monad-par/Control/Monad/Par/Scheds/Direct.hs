{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP, ScopedTypeVariables,
             TypeSynonymInstances, MultiParamTypeClasses,
             GeneralizedNewtypeDeriving, PackageImports
	     #-}

{- OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind -}

-- {- LANGUAGE Trustworthy -}
-- TODO: Before declaring this module TRUSTWORTHY/SAFE, we need to
-- make the IVar type abstract.

-- | A scheduler for the Par monad based on directly performing IO
-- actions when Par methods are called (i.e. without using a lazy
-- trace data structure).

module Control.Monad.Par.Scheds.Direct (
   Sched(..), 
   Par, -- abstract: Constructor not exported.
   IVar(..), IVarContents(..),
--    sched,
    runPar, runParIO,
    new, get, put_, fork,
    newFull, newFull_, put,
    spawn, spawn_, spawnP,
    spawn1_
--   runParAsync, runParAsyncHelper,
--   yield,
 ) where

import Control.Applicative
import Control.Concurrent hiding (yield)
import Data.IORef         (IORef,newIORef,readIORef,writeIORef,atomicModifyIORef)
import Text.Printf        (printf, hPrintf)
import GHC.Conc           (numCapabilities,yield)
import           "mtl" Control.Monad.Cont as C
import qualified "mtl" Control.Monad.Reader as RD
import qualified       System.Random.MWC as Random
import                 System.IO  (stderr)
import                 System.IO.Unsafe (unsafePerformIO)
import                 System.Mem.StableName (makeStableName, hashStableName)
import qualified       Control.Monad.Par.Class  as PC
import qualified       Control.Monad.Par.Unsafe as UN
import                 Control.Monad.Par.Scheds.DirectInternal
                       (Par(..), Sched(..), HotVar, 
                        newHotVar, readHotVar, modifyHotVar, writeHotVarRaw)
import Control.DeepSeq
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Concurrent.Deque.Class (WSDeque)
import Data.Concurrent.Deque.Reference.DequeInstance
import Data.Concurrent.Deque.Reference as R

import qualified Control.Exception as E
import qualified Control.Concurrent.Async as A

import Prelude hiding (null)
import qualified Prelude

--------------------------------------------------------------------------------
-- Configuration Toggles
--------------------------------------------------------------------------------

#define DEBUG
-- [2012.08.30] This shows a 10X improvement on nested parfib:
-- #define NESTED_SCHEDS
#define PARPUTS
-- #define FORKPARENT
-- #define IDLING_ON
   -- Next, IF idling is on, should we do wakeups?:
-- #define WAKEIDLE

-------------------------------------------------------------------
-- Ifdefs for the above preprocessor defines.  Try to MINIMIZE code
-- that lives in this dangerous region, and instead do normal
-- conditionals and trust dead-code-elimination.
--------------------------------------------------------------------

#ifdef DEBUG
import Debug.Trace        (trace)
import System.Environment (getEnvironment)
theEnv = unsafePerformIO $ getEnvironment
dbg = True
dbglvl = 1
#else
dbg = False
dbglvl = 0
#endif
dbg    :: Bool
dbglvl :: Int

_PARPUTS :: Bool
#ifdef PARPUTS
_PARPUTS = True
#else
_PARPUTS = False
#endif

_FORKPARENT :: Bool
#ifdef FORKPARENT
_FORKPARENT = True
#else
#warning "FORKPARENT POLICY NOT USED; THIS IS GENERALLY WORSE"
_FORKPARENT = False
#endif

_IDLING_ON :: Bool
#ifdef IDLING_ON
_IDLING_ON = True
#else
_IDLING_ON = False
#endif


--------------------------------------------------------------------------------
-- Core type definitions
--------------------------------------------------------------------------------

type ROnly = RD.ReaderT Sched IO

newtype IVar a = IVar (IORef (IVarContents a))

data IVarContents a = Full a | Empty | Blocked [a -> IO ()]

unsafeParIO :: IO a -> Par a 
unsafeParIO iom = Par (lift$ lift iom)

io :: IO a -> Par a
io = unsafeParIO -- shorthand used below

--------------------------------------------------------------------------------
-- Global State
--------------------------------------------------------------------------------

-- This keeps track of ALL worker threads across all unreated
-- `runPar` instantiations.  This is used to detect nested invocations
-- of `runPar` and avoid reinitialization.
-- globalWorkerPool :: IORef (Data.IntMap ())
globalWorkerPool :: IORef (M.Map ThreadId Sched)
globalWorkerPool = unsafePerformIO $ newIORef M.empty
-- TODO! Make this semi-local! (not shared between "top-level" runPars)

{-# INLINE amINested #-}
{-# INLINE registerWorker #-}
{-# INLINE unregisterWorker #-}
amINested :: ThreadId -> IO (Maybe Sched)
registerWorker :: ThreadId -> Sched -> IO ()
unregisterWorker :: ThreadId -> IO ()
#ifdef NESTED_SCHEDS
-- | If the current threadID is ALREADY a worker, return the corresponding Sched structure.
amINested tid = do
  -- There is no race here.  Each thread inserts itself before it
  -- becomes an active worker.
  wp <- readIORef globalWorkerPool
  return (M.lookup tid wp)
registerWorker tid sched = 
  atomicModifyIORef globalWorkerPool $ 
    \ mp -> (M.insert tid sched mp, ())
unregisterWorker tid = 
  atomicModifyIORef globalWorkerPool $ 
    \ mp -> (M.delete tid mp, ())
#else 
amINested      _      = return Nothing
registerWorker _ _    = return ()
unregisterWorker _tid = return ()
#endif

-----------------------------------------------------------------------------
-- Helpers #2:  Pushing and popping work.
-----------------------------------------------------------------------------

{-# INLINE popWork  #-}
popWork :: Sched -> IO (Maybe (Par ()))
popWork Sched{ workpool, no } = do 
  mb <- R.tryPopL workpool 
  when dbg $ case mb of 
         Nothing -> return ()
	 Just _  -> do sn <- makeStableName mb
	 	       printf " [%d]                                   -> POP work unit %d\n" no (hashStableName sn)
  return mb

{-# INLINE pushWork #-}
pushWork :: Sched -> Par () -> IO ()
pushWork Sched { workpool, idle, no, isMain } task = do
  R.pushL workpool task
  when dbg $ do sn <- makeStableName task
		printf " [%d]                                   -> PUSH work unit %d\n" no (hashStableName sn)
#if  defined(IDLING_ON) && defined(WAKEIDLE)
  --when isMain$    -- Experimenting with reducing contention by doing this only from a single thread.
                    -- TODO: We need to have a proper binary wakeup-tree.
  tryWakeIdle idle
#endif
  return ()

tryWakeIdle :: HotVar [MVar Bool] -> IO ()
tryWakeIdle idle = do
-- NOTE: I worry about having the idle var hammmered by all threads on their spawn-path:
  -- If any worker is idle, wake one up and give it work to do.
  idles <- readHotVar idle -- Optimistically do a normal read first.
  when (not (Prelude.null idles)) $ do
    when dbg$ printf "Waking %d idle thread(s).\n" (length idles)
    r <- modifyHotVar idle (\is -> case is of
                             []      -> ([], return ())
                             (i:ils) -> (ils, putMVar i False))
    r -- wake an idle worker up by putting an MVar.

rand :: HotVar Random.GenIO -> IO Int
rand ref = Random.uniformR (0, numCapabilities-1) =<< readHotVar ref

--------------------------------------------------------------------------------
-- Running computations in the Par monad
--------------------------------------------------------------------------------

instance NFData (IVar a) where
  rnf _ = ()

{-# NOINLINE runPar #-}
runPar = unsafePerformIO . runParIO

runParIO userComp = do
   tid <- myThreadId  
#if __GLASGOW_HASKELL__ >= 701 /* 20110301 */
    --
    -- We create a thread on each CPU with forkOn.  The CPU on which
    -- the current thread is running will host the main thread; the
    -- other CPUs will host worker threads.
    --
    -- Note: GHC 7.1.20110301 is required for this to work, because that
    -- is when threadCapability was added.
    --
   (main_cpu, _) <- threadCapability tid
#else
    --
    -- Lacking threadCapability, we always pick CPU #0 to run the main
    -- thread.  If the current thread is not running on CPU #0, this
    -- will require some data to be shipped over the memory bus, and
    -- hence will be slightly slower than the version above.
    --
   let main_cpu = 0
#endif
   maybSched <- amINested tid
   case maybSched of 
     Just (sched) -> do
       sid <- modifyHotVar (sessionCounter sched) (\ x -> (x+1,x))
       when (dbglvl>=1)$ printf " [%d %s] runPar called from existing worker thread, new session (%d)....\n" (no sched) (show tid) sid
       -- Here the current thread is ALREADY a worker.  All we need to
       -- do is plug the users new computation in.
--       runReaderWith sched $ rescheduleR errK

       -- Here we have an extra IORef... ugly.
       ref <- newIORef (error "this should never be touched")

       _ <- modifyHotVar (activeSessions sched) (\ set -> (S.insert sid set, ()))
       newSess <- newHotVar False
       let kont ans = liftIO$ do when (dbglvl>=1) $ do
                                   tid2 <- myThreadId
                                   printf " [%d %s] Continuation for nested session called, finishing it up (%d)...\n" (no sched) (show tid2) sid
                                 writeIORef ref ans
                                 writeHotVarRaw newSess True
                                 modifyHotVar (activeSessions sched) (\ set -> (S.delete sid set, ()))
                                 
       runReaderWith (sched{ sessionFinished=newSess}) (C.runContT (unPar userComp) kont) 
       when (dbglvl>=1)$ do
         active <- readHotVar (activeSessions sched)
         printf " [%d %s] RETURN from nested runContT (%d) active set %s\n" (no sched) (show tid) sid (show active)
       -- By returning here we ARE implicitly reengaging the scheduler, since we
       -- are already inside the rescheduleR loop on this thread
       -- (before runParIO was called in a nested fashion).
       readIORef ref
     Nothing -> do
       allscheds <- makeScheds main_cpu
       
       tidorig <- myThreadId -- TODO: remove when done debugging
       
       mfin <- newEmptyMVar
       doneFlags <- forM (zip [0..] allscheds) $ \(cpu,sched) -> do
            workerDone <- newEmptyMVar            
            ----------------------------------------
            let wname = ("(worker of originator "++show tidorig++")")
--            forkOn cpu $ do
            forkWithExceptions (forkOn cpu) wname $ do                                    
--            as <- A.asyncOn cpu $ do
--            as <- A.async $ do            
            ------------------------------------------------------------STRT WORKER THREAD              
              tid2 <- myThreadId
              registerWorker tid2 sched
              if (cpu /= main_cpu)
                 then do when dbg$ printf " [%d %s] Entering scheduling loop.\n" cpu (show tid2)
                         runReaderWith sched $ rescheduleR (trivialCont wname)
                         when dbg$ printf " [%d] Exited scheduling loop.  FINISHED.\n" cpu
                         putMVar workerDone cpu
                         return ()
                 else do
                      let userComp'  = do when dbg$ io$ printf " [%d %s] Starting Par computation on main thread.\n" main_cpu (show tid2)
                                          res <- userComp
                                          finalSched <- RD.ask 
                                          io$ writeIORef (killflag sched) True
                                          when dbg$ io$ do tid3 <- myThreadId
                                                           printf " *** [%d %s] Out of Par on main thread. Set killflag; next write MVar !!\n"
                                                                  (no finalSched) (show tid3)
                                          -- Sanity check our work queues:
--                                          when dbg $ io$ sanityCheck allscheds
                                          io$ putMVar mfin res

                      runReaderWith sched (C.runContT (unPar userComp') (trivialCont "main worker"))
                      when dbg$ do printf " *** Out of entire runContT user computation on main thread %s.\n" (show tid2)
--                                   sanityCheck allscheds
                      -- Not currently requiring that other scheduler threads have exited before we 
                      -- (the main thread) exits (FIXME).  But we do signal here that they should terminate:
--                      writeIORef (killflag sched) True
              unregisterWorker tid
--              return cpu
            ------------------------------------------------------------END WORKER THREAD
--            return as
            return workerDone
#if 1
       when dbg$ printf " *** [%s] Originator thread: waiting for workers to complete." (show tidorig)
       forM_ doneFlags $ \ mv -> do 
         n <- readMVar mv
--         n <- tryTakeMVar mv 
--         n <- A.wait mv
         when dbg$ printf "   * [%s]  Worker %s completed\n" (show tidorig) (show n)
#endif

       when dbg$ do printf " *** [%s] Reading final MVar on originator thread." (show tidorig)  
       -- We don't directly use the thread we come in on.  Rather, that thread waits
       -- waits.  One reason for this is that the main/progenitor thread in
       -- GHC is expensive like a forkOS thread.
       ----------------------------------------
       --              DEBUGGING             -- 
--       takeMVar mfin -- Final value.
       dbgTakeMVar "global waiting thread" mfin -- Final value.
--       busyTakeMVar (" The global wait "++ show tidorig) mfin -- Final value.                    
       ----------------------------------------

-- Create the default scheduler(s) state:
makeScheds :: Int -> IO [Sched]
makeScheds main = do   
   when dbg$ do tid <- myThreadId
                printf "[initialization] Creating %d worker threads, currently on %s\n" numCapabilities (show tid)
   workpools <- replicateM numCapabilities $ R.newQ
   rngs      <- replicateM numCapabilities $ Random.create >>= newHotVar 
   idle      <- newHotVar []   
   killflag  <- newHotVar False
   sessionFinished <- newHotVar False

   activeSessions  <- newHotVar S.empty
   sessionCounter  <- newHotVar 1000
   let allscheds = [ Sched { no=x, idle, killflag, isMain= (x==main),
			     workpool=wp, scheds=allscheds, rng=rng,
                             sessionFinished=sessionFinished,
                             activeSessions=activeSessions,
                             sessionCounter=sessionCounter
			   }
                   | (x,wp,rng) <- zip3 [0..] workpools rngs]
   return allscheds



--------------------------------------------------------------------------------
-- IVar operations
--------------------------------------------------------------------------------

{-# INLINE new  #-}
-- | creates a new @IVar@
new :: Par (IVar a)
new  = io$ do r <- newIORef Empty
              return (IVar r)

{-# INLINE get  #-}
-- | read the value in a @IVar@.  The 'get' can only return when the
-- value has been written by a prior or parallel @put@ to the same
-- @IVar@.
get (IVar vr) =  do 
  callCC $ \kont -> 
    do
       e  <- io$ readIORef vr
       case e of
	  Full a -> return a
	  _ -> do
            sch <- RD.ask
#  ifdef DEBUG
            sn <- io$ makeStableName vr  -- Should probably do the MutVar inside...
            let resched = trace (" ["++ show (no sch) ++ "]  - Rescheduling on unavailable ivar "++show (hashStableName sn)++"!") 
#else
            let resched = 
#  endif
			  longjmpSched -- Invariant: kont must not be lost.
            -- Because we continue on the same processor the Sched stays the same:
            -- TODO: Try NOT using monadic values as first class.  Check for performance effect:
	    r <- io$ atomicModifyIORef vr $ \x -> case x of
		      Empty      -> (Blocked [pushWork sch . kont], resched)
		      Full a     -> (Full a, return a) -- kont is implicit here.
		      Blocked ks -> (Blocked (pushWork sch . kont:ks), resched)
	    r

-- | NOTE unsafePeek is NOT exposed directly through this module.  (So
-- this module remains SAFE in the Safe Haskell sense.)  It can only
-- be accessed by importing Control.Monad.Par.Unsafe.
{-# INLINE unsafePeek #-}
unsafePeek :: IVar a -> Par (Maybe a)
unsafePeek (IVar v) = do 
  e  <- io$ readIORef v
  case e of 
    Full a -> return (Just a)
    _      -> return Nothing

------------------------------------------------------------
{-# INLINE put_ #-}
-- | @put_@ is a version of @put@ that is head-strict rather than fully-strict.
--   In this scheduler, puts immediately execute woken work in the current thread.
put_ (IVar vr) !content = do
   sched <- RD.ask
   ks <- io$ do 
      ks <- atomicModifyIORef vr $ \e -> case e of
               Empty      -> (Full content, [])
               Full _     -> error "multiple put"
               Blocked ks -> (Full content, ks)
#ifdef DEBUG
      when (dbglvl >=  3) $ do 
         sn <- makeStableName vr
         printf " [%d] Put value %s into IVar %d.  Waking up %d continuations.\n" 
                (no sched) (show content) (hashStableName sn) (length ks)
         return ()
#endif 
      return ks
   wakeUp sched ks content   

-- | NOTE unsafeTryPut is NOT exposed directly through this module.  (So
-- this module remains SAFE in the Safe Haskell sense.)  It can only
-- be accessed by importing Control.Monad.Par.Unsafe.
{-# INLINE unsafeTryPut #-}
unsafeTryPut (IVar vr) !content = do
   -- Head strict rather than fully strict.
   sched <- RD.ask 
   (ks,res) <- io$ do 
      pr <- atomicModifyIORef vr $ \e -> case e of
		   Empty      -> (Full content, ([], content))
		   Full x     -> (Full x, ([], x))
		   Blocked ks -> (Full content, (ks, content))
#ifdef DEBUG
      sn <- makeStableName vr
      printf " [%d] unsafeTryPut: value %s in IVar %d.  Waking up %d continuations.\n" 
	     (no sched) (show content) (hashStableName sn) (length (fst pr))
#endif
      return pr
   wakeUp sched ks content
   return res

-- | When an IVar is filled in, continuations wake up.
{-# INLINE wakeUp #-}
wakeUp :: Sched -> [a -> IO ()]-> a -> Par ()
wakeUp _sched ks arg = loop ks
 where
   loop [] = return ()
   loop (kont:rest) = do
     -- FIXME -- without strict firewalls keeping ivars from moving
     -- between runPar sessions, if we allow nested scheduler use
     -- we could potentially wake up work belonging to a different
     -- runPar and thus bring it into our worker and delay our own
     -- continuation until its completion.
     if _PARPUTS then
       -- We do NOT force the putting thread to postpone its continuation.
       do spawn_$ pMap kont rest
          return ()
       -- case rest of
       --   [] -> spawn_$ io$ kont arg
       --   _  -> spawn_$ do spawn_$ io$ kont arg
       --                    io$ parchain rest
       -- error$"FINISHME - wake "++show (length ks)++" conts"
      else 
       -- This version sacrifices a parallelism opportunity and
       -- imposes additional serialization.
       --
       -- [2012.08.31] WARNING -- this serialzation CAN cause deadlock.
       -- This "optimization" should not be on the table.
       -- mapM_ ($arg) ks
       do io$ kont arg
          loop rest 
     return ()

   pMap kont [] = io$ kont arg
   pMap kont (more:rest) =
     do spawn_$ io$ kont arg
        pMap more rest

   -- parchain [kont] = kont arg
   -- parchain (kont:rest) = do spawn$ io$ kont arg
   --                           parchain rest
                              

------------------------------------------------------------
-- TODO: Continuation (parent) stealing version.
{-# INLINE fork #-}
fork :: Par () -> Par ()
fork task = 
  case _FORKPARENT of 
    True -> do 
      sched <- RD.ask   
      callCC$ \parent -> do
         let wrapped = parent ()
         io$ pushWork sched wrapped
         -- Then execute the child task and return to the scheduler when it is complete:
         task 
         -- If we get to this point we have finished the child task:
         longjmpSched -- We reschedule to pop the cont we pushed.
         io$ putStrLn " !!! ERROR: Should never reach this point #1" 

      when dbg$ do 
       sched2 <- RD.ask 
       io$ printf "  -  called parent continuation... was on worker [%d] now on worker [%d]\n" (no sched) (no sched2)
       return ()

    False -> do 
      sch <- RD.ask
      when dbg$ io$ printf " [%d] forking task...\n" (no sch)
      io$ pushWork sch task
   
-- This routine "longjmp"s to the scheduler, throwing out its own continuation.
longjmpSched :: Par a
-- longjmpSched = Par $ C.ContT rescheduleR
longjmpSched = Par $ C.ContT (\ _k -> rescheduleR (trivialCont "longjmpSched"))

-- Reschedule the scheduler loop until it observes killflag==True, and
-- then it finally invokes its continuation.
rescheduleR :: (a -> ROnly ()) -> ROnly ()
rescheduleR kont = do
  mysched <- RD.ask 
  when dbg$ liftIO$ do tid <- myThreadId
                       k <- liftIO$ readIORef (killflag mysched)
                       fin <- liftIO$ readIORef (sessionFinished mysched)
                       printf " [%d %s]  - Reschedule... kill %s sessfin %s\n" (no mysched) (show tid) (show k) (show fin)
  mtask  <- liftIO$ popWork mysched
  case mtask of
    Nothing -> do 
                  k <- liftIO$ readIORef (killflag mysched)
                  fin <- liftIO$ readIORef (sessionFinished mysched)
		  if (k ||  fin) 
                   then do when (dbglvl >= 1) $ liftIO $ do
                             tid <- myThreadId
                             printf " [%d %s]  - DROP out of reschedule loop, killflag=%s, sessionFinished=%s\n" 
                                    (no mysched) (show tid) (show k) (show fin)
                           kont (error "Direct.hs: The result value from rescheduleR should not be used.")
                   else do
		     liftIO$ steal mysched
#ifdef WAKEIDLE
--                     io$ tryWakeIdle (idle mysched)
#endif
                     liftIO yield
		     rescheduleR kont
    Just task -> do
       -- When popping work from our own queue the Sched (Reader value) stays the same:
       when dbg $ do sn <- liftIO$ makeStableName task
		     liftIO$ printf " [%d] popped work %d from own queue\n" (no mysched) (hashStableName sn)
       let C.ContT fn = unPar task 
       -- Run the stolen task with a continuation that returns to the scheduler if the task exits normally:
       fn (\ _ -> do 
           sch <- RD.ask
           when dbg$ liftIO$ printf "  + task finished successfully on cpu %d, calling reschedule continuation..\n" (no sch)
	   rescheduleR kont)


-- | Attempt to steal work or, failing that, give up and go idle.
-- 
--   The current policy is to do a burst of of N tries without
--   yielding or pausing inbetween.
steal :: Sched -> IO ()
steal mysched@Sched{ idle, scheds, rng, no=my_no } = do
  when (dbglvl>=2)$ printf " [%d]  + stealing\n" my_no
  i <- getnext (-1 :: Int)
  go maxtries i
 where
--    maxtries = numCapabilities -- How many times should we attempt theft before going idle?
    maxtries = 20 * numCapabilities -- How many times should we attempt theft before going idle?

    getnext _ = rand rng

    ----------------------------------------
    -- IDLING behavior:
    go 0 _ | _IDLING_ON = 
            do m <- newEmptyMVar
               r <- modifyHotVar idle $ \is -> (m:is, is)
               if length r == numCapabilities - 1
                  then do
                     when dbg$ printf " [%d]  | initiating shutdown\n" my_no
                     mapM_ (\vr -> putMVar vr True) r
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

    -- We need to return from this loop to check killflag and exit the scheduler if necessary.
    go 0 _i | _IDLING_ON == False = yield

    ----------------------------------------
    go tries i
      | i == my_no = do i' <- getnext i
			go (tries-1) i'

      | otherwise     = do
         -- We ONLY go through the global sched array to access 
         let schd = scheds!!i
         when (dbglvl>=2)$ printf " [%d]  | trying steal from %d\n" my_no (no schd)

--         let dq = workpool schd :: WSDeque (Par ())
         let dq = workpool schd 
         r <- R.tryPopR dq

         case r of
           Just task  -> do
              when dbg$ do sn <- makeStableName task
			   printf " [%d]  | stole work (unit %d) from cpu %d\n" my_no (hashStableName sn) (no schd)
	      runReaderWith mysched $ 
		C.runContT (unPar task)
		 (\_ -> do
		   when dbg$ do sn <- liftIO$ makeStableName task
		                liftIO$ printf " [%d]  | DONE running stolen work (unit %d) from %d\n" my_no (hashStableName sn) (no schd)
		   return ())

           Nothing -> do i' <- getnext i
			 go (tries-1) i'

-- | The continuation which should not be called.
errK :: t
errK = error "Error cont: this closure shouldn't be used"

trivialCont :: String -> a -> ROnly ()
trivialCont str _ = do 
#ifdef DEBUG
--                trace (str ++" trivialCont evaluated!")
                liftIO$ printf " !! trivialCont evaluated, msg: %s\n" str
#endif
		return ()

----------------------------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- <boilerplate>

-- TEMP: TODO: Factor out this boilerplate somehow.

{-# INLINE spawn1_ #-}
-- Spawn a one argument function instead of a thunk.  This is good for debugging if the value supports "Show".
spawn1_ f x = 
#ifdef DEBUG
 do sn  <- io$ makeStableName f
    sch <- RD.ask; when dbg$ io$ printf " [%d] spawning fn %d with arg %s\n" (no sch) (hashStableName sn) (show x)
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
spawnP :: (Show a, NFData a) => a -> Par (IVar a)
put_   :: Show a => IVar a -> a -> Par ()
get    :: Show a => IVar a -> Par a
runPar :: Show a => Par a -> a 
runParIO :: Show a => Par a -> IO a 
newFull :: (Show a, NFData a) => a -> Par (IVar a)
newFull_ ::  Show a => a -> Par (IVar a)
unsafeTryPut :: Show b => IVar b -> b -> Par b
#else
spawn  :: NFData a => Par a -> Par (IVar a)
spawn_ :: Par a -> Par (IVar a)
spawn1_ :: (a -> Par b) -> a -> Par (IVar b)
spawnP :: NFData a => a -> Par (IVar a)
put_   :: IVar a -> a -> Par ()
put    :: NFData a => IVar a -> a -> Par ()
get    :: IVar a -> Par a
runPar :: Par a -> a 
runParIO :: Par a -> IO a 
newFull :: NFData a => a -> Par (IVar a)
newFull_ ::  a -> Par (IVar a)
unsafeTryPut :: IVar b -> b -> Par b

-- We can't make proper instances with the extra Show constraints:
instance PC.ParFuture IVar Par  where
  get    = get
  spawn  = spawn
  spawn_ = spawn_
  spawnP = spawnP

instance PC.ParIVar IVar Par  where
  fork = fork
  new  = new
  put_ = put_
  newFull = newFull
  newFull_ = newFull_

instance UN.ParUnsafe IVar Par  where
  unsafePeek   = unsafePeek
  unsafeTryPut = unsafeTryPut
  unsafeParIO  = unsafeParIO
#endif

instance Functor Par where
   fmap f xs = xs >>= return . f

instance Applicative Par where
   (<*>) = ap
   pure  = return
-- </boilerplate>
--------------------------------------------------------------------------------


{-# INLINE runReaderWith #-}
-- | Arguments flipped for convenience.
runReaderWith :: r -> RD.ReaderT r m a -> m a
runReaderWith state m = RD.runReaderT m state


--------------------------------------------------------------------------------
-- DEBUGGING TOOLs
--------------------------------------------------------------------------------

-- Make sure there is no work left in any deque after exiting.
sanityCheck :: [Sched] -> IO ()
sanityCheck allscheds = do
  forM_ allscheds $ \ Sched{no, workpool} -> do
     b <- R.nullQ workpool
     when (not b) $ do 
         () <- printf "WARNING: After main thread exited non-empty queue remains for worker %d\n" no
         return ()
  putStrLn "Sanity check complete."


-- | This tries to localize the blocked-indefinitely exception:
dbgTakeMVar :: String -> MVar a -> IO a
dbgTakeMVar msg mv = 
--  catch (takeMVar mv) ((\_ -> doDebugStuff) :: BlockedIndefinitelyOnMVar -> IO a)
  E.catch (takeMVar mv) ((\_ -> doDebugStuff) :: IOError -> IO a)
 where   
   doDebugStuff = do putStrLn$"This takeMVar blocked indefinitely!: "++msg
                     error "failed"

-- | For debugging purposes.  This can help us figure out (but an ugly
--   process of elimination) which MVar reads are leading to a "Thread
--   blocked indefinitely" exception.
busyTakeMVar :: String -> MVar a -> IO a
busyTakeMVar msg mv = try (10 * 1000 * 1000)
 where 
 try 0 = do 
   tid <- myThreadId
   -- After we've failed enough times, start complaining:
   printf "%s not getting anywhere, msg: %s\n" (show tid) msg  
   try (100 * 1000)
 try n = do
   x <- tryTakeMVar mv
   case x of 
     Just y  -> return y
     Nothing -> do yield; try (n-1)
   

-- | Fork a thread but ALSO set up an error handler that suppresses
--   MVar exceptions.
forkIO_Suppress :: Int -> IO () -> IO ThreadId
forkIO_Suppress whre action = 
  forkOn whre $ 
           E.handle (\e -> 
                      case (e :: E.BlockedIndefinitelyOnMVar) of
                       _ -> do 
                               putStrLn$"CAUGHT child thread exception: "++show e 
                               return ()
		    )
           action

-- forkOnIt


-- | Exceptions that walk up the fork tree of threads:
forkWithExceptions :: (IO () -> IO ThreadId) -> String -> IO () -> IO ThreadId
forkWithExceptions forkit descr action = do 
   parent <- myThreadId
   forkit $ 
      E.catch action
	 (\ e -> 
           case E.fromException e of 
             Just E.ThreadKilled -> printf -- hPrintf stderr 
                                    "ThreadKilled exception inside child thread (not propagating!): %s\n" (show descr)
	     _  -> do printf -- hPrintf stderr
                        "Exception inside child thread %s: %s\n" (show descr) (show e)
                      E.throwTo parent (e :: E.SomeException)
	 )
