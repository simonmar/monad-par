{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP, ScopedTypeVariables,
             TypeSynonymInstances, MultiParamTypeClasses,
             GeneralizedNewtypeDeriving, PackageImports,
             ParallelListComp #-}


{- OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind -}

-- {- LANGUAGE Trustworthy -}
-- TODO: Before declaring this module TRUSTWORTHY/SAFE, we need to
-- make the IVar type abstract.

{-# LANGUAGE TypeFamilies #-}

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
    spawn1_, fixPar, FixParException (..)
--   runParAsync, runParAsyncHelper,
--   yield,
 ) where

import Control.Applicative
import Control.Concurrent hiding (yield)
import Data.IORef         (IORef,newIORef,readIORef,writeIORef,atomicModifyIORef)
import Text.Printf        (printf)
import GHC.Conc           (numCapabilities,yield)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import           "mtl" Control.Monad.Cont as C
import qualified "mtl" Control.Monad.Reader as RD
import qualified       System.Random.MWC as Random
import                 System.IO.Unsafe (unsafePerformIO)
import                 System.Mem.StableName (makeStableName, hashStableName)
import qualified       Control.Monad.Par.Class  as PC
import qualified       Control.Monad.Par.Unsafe as UN
import                 Control.Monad.Par.Scheds.DirectInternal
                       (Par(..), Sched(..), HotVar, SessionID, Session(Session),
                        newHotVar, readHotVar, modifyHotVar, modifyHotVar_,
                        writeHotVarRaw, fixPar, FixParException (..))
#ifdef NEW_GENERIC
import qualified       Control.Par.Class as PN
import qualified       Control.Par.Class.Unsafe as PU
#endif
import Control.DeepSeq
#ifdef NESTED_SCHEDS
import qualified Data.Map as M
#endif
import qualified Data.Set as S
import Data.Maybe (catMaybes)
import Data.Word (Word64)

-- import Data.Concurrent.Deque.Class (WSDeque)
#ifdef USE_CHASELEV
#warning "Note: using Chase-Lev lockfree workstealing deques..."
import Data.Concurrent.Deque.ChaseLev.DequeInstance
import Data.Concurrent.Deque.ChaseLev as R
#else
import Data.Concurrent.Deque.Reference.DequeInstance
import Data.Concurrent.Deque.Reference as R
#endif

import qualified Control.Exception as E

import Prelude hiding (null)
import qualified Prelude

#if __GLASGOW_HASKELL__ <= 700
import GHC.Conc (forkOnIO)
forkOn = forkOnIO
#endif

--------------------------------------------------------------------------------
-- Configuration Toggles
--------------------------------------------------------------------------------

-- [2012.08.30] This shows a 10X improvement on nested parfib:
-- #define NESTED_SCHEDS
#define PARPUTS
#define FORKPARENT
#define IDLING_ON
   -- Next, IF idling is on, should we do wakeups?:
#define WAKEIDLE

-- #define WAIT_FOR_WORKERS

-------------------------------------------------------------------
-- Ifdefs for the above preprocessor defines.  Try to MINIMIZE code
-- that lives in this dangerous region, and instead do normal
-- conditionals and trust dead-code-elimination.
--------------------------------------------------------------------

#ifdef DEBUG_DIRECT
#warning "DEBUG: Activating debugging for Direct.hs"
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

_WAIT_FOR_WORKERS :: Bool
#ifdef WAIT_FOR_WORKERS
_WAIT_FOR_WORKERS = True
#else
_WAIT_FOR_WORKERS = False
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
#ifdef NESTED_SCHEDS
globalWorkerPool :: IORef (M.Map ThreadId Sched)
globalWorkerPool = unsafePerformIO $ newIORef M.empty
#endif
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
pushWork Sched { workpool, idle, no } task = do
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
-- NOTE: I worry about having the idle var hammered by all threads on their spawn-path:
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
  rnf !_ = ()

{-# NOINLINE runPar #-}
runPar = unsafePerformIO . runParIO


-- | This procedure creates a new worker on the current thread (with a
--   new session ID) and plugs it into the work-stealing environment.
--   This new worker extracts itself from the work stealing pool when
--   `userComp` has completed, thus freeing the current thread (this
--   procedure) to return normally.
runNewSessionAndWait :: String -> Sched -> Par b -> IO b
runNewSessionAndWait name sched userComp = do
    tid <- myThreadId -- TODO: remove when done debugging
    sid <- modifyHotVar (sessionCounter sched) (\ x -> (x+1,x))
    _ <- modifyHotVar (activeSessions sched) (\ set -> (S.insert sid set, ()))

    -- Here we have an extra IORef... ugly.
    ref <- newIORef (error$ "Empty session-result ref ("++name++") should never be touched (sid "++ show sid++", "++show tid ++")")
    newFlag <- newHotVar False
    -- Push the new session:
    _ <- modifyHotVar (sessions sched) (\ ls -> ((Session sid newFlag) : ls, ()))

    let userComp' = do when dbg$ io$ do
                           tid2 <- myThreadId
                           printf " [%d %s] Starting Par computation on %s.\n" (no sched) (show tid2) name
                       ans <- userComp
                       -- This add-on to userComp will run only after userComp has completed successfully,
                       -- but that does NOT guarantee that userComp-forked computations have terminated:
                       io$ do when (dbglvl>=1) $ do
                                tid3 <- myThreadId
                                printf " [%d %s] Continuation for %s called, finishing it up (%d)...\n" (no sched) (show tid3) name sid
                              writeIORef ref ans
                              writeHotVarRaw newFlag True
                              modifyHotVar (activeSessions sched) (\ set -> (S.delete sid set, ()))
        kont :: Word64 -> a -> ROnly ()
        kont n = trivialCont$ "("++name++", sid "++show sid++", round "++show n++")"
        loop :: Word64 -> ROnly ()
        loop n = do flg <- liftIO$ readIORef newFlag
                    unless flg $ do
                      when dbg $ liftIO$ do
                        tid4 <- myThreadId
                        printf " [%d %s] BOUNCE %d... going into reschedule until finished.\n" (no sched) (show tid4) n
                      rescheduleR 0 $ trivialCont$ "("++name++", sid "++show sid++")"
                      loop (n+1)

    -- THIS IS RETURNING TOO EARLY!!:
    runReaderWith sched (C.runContT (unPar userComp') (kont 0))  -- Does this ASSUME child stealing?
    runReaderWith sched (loop 1)

    -- TODO: Ideally we would wait for ALL outstanding (stolen) work on this "team" to complete.

    when (dbglvl>=1)$ do
      active <- readHotVar (activeSessions sched)
      sess@True <- readHotVar newFlag -- ASSERT!
      printf " [%d %s] RETURN from %s (sessFin %s) runContT (%d) active set %s\n"
               (no sched) (show tid) name (show sess) sid (show active)

    -- Here we pop off the frame we added to the session stack:
    modifyHotVar_ (sessions sched) $ \ (Session sid2 _ : tl) ->
        if sid == sid2
        then tl
        else error$ "Tried to pop the session stack and found we ("++show sid
                   ++") were not on the top! (instead "++show sid2++")"

    -- By returning here we ARE implicitly reengaging the scheduler, since we
    -- are already inside the rescheduleR loop on this thread
    -- (before runParIO was called in a nested fashion).
    readIORef ref


{-# NOINLINE runParIO #-}
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
   tidorig <- myThreadId -- TODO: remove when done debugging
   case maybSched of
     Just (sched) -> do
       -- Here the current thread is ALREADY a worker.  All we need to
       -- do is plug the users new computation in.

       sid0 <- readHotVar (sessionCounter sched)
       when (dbglvl>=1)$ printf " [%d %s] runPar called from existing worker thread, new session (%d)....\n" (no sched) (show tid) (sid0 + 1)
       runNewSessionAndWait "nested runPar" sched userComp

     ------------------------------------------------------------
     -- Non-nested case, make a new set of worker threads:
     ------------------------------------------------------------
     Nothing -> do
       allscheds <- makeScheds main_cpu
       [Session _ topSessFlag] <- readHotVar$ sessions$ head allscheds

       mfin <- newEmptyMVar
       doneFlags <- forM (zip [0..] allscheds) $ \(cpu,sched) -> do
            workerDone <- newEmptyMVar
            ----------------------------------------
            let wname = ("(worker "++show cpu++" of originator "++show tidorig++")")
--            forkOn cpu $ do
            _ <- forkWithExceptions (forkOn cpu) wname $ do
            ------------------------------------------------------------STRT WORKER THREAD
              tid2 <- myThreadId
              registerWorker tid2 sched
              if (cpu /= main_cpu)
                 then do when dbg$ printf " [%d %s] Anonymous worker entering scheduling loop.\n" cpu (show tid2)
                         runReaderWith sched $ rescheduleR 0 (trivialCont (wname++show tid2))
                         when dbg$ printf " [%d] Anonymous worker exited scheduling loop.  FINISHED.\n" cpu
                         putMVar workerDone cpu
                         return ()
                 else do x <- runNewSessionAndWait "top-lvl main worker" sched userComp
                         -- When the main worker finishes we can tell the anonymous "system" workers:
                         writeIORef topSessFlag True
                         when dbg$ do printf " *** Out of entire runContT user computation on main thread %s.\n" (show tid2)
                         --  sanityCheck allscheds
                         putMVar mfin x

              unregisterWorker tid
            ------------------------------------------------------------END WORKER THREAD
            return (if cpu == main_cpu then Nothing else Just workerDone)

       when _WAIT_FOR_WORKERS $ do
           when dbg$ printf " *** [%s] Originator thread: waiting for workers to complete." (show tidorig)
           forM_ (catMaybes doneFlags) $ \ mv -> do
             n <- readMVar mv
    --         n <- A.wait mv
             when dbg$ printf "   * [%s]  Worker %s completed\n" (show tidorig) (show n)

       when dbg$ do printf " *** [%s] Reading final MVar on originator thread.\n" (show tidorig)
       -- We don't directly use the thread we come in on.  Rather, that thread waits
       -- waits.  One reason for this is that the main/progenitor thread in
       -- GHC is expensive like a forkOS thread.
       ----------------------------------------
       --              DEBUGGING             --
#ifdef DEBUG_DIRECT
       busyTakeMVar (" The global wait "++ show tidorig) mfin -- Final value.
--       dbgTakeMVar "global waiting thread" mfin -- Final value.
#else
       takeMVar mfin -- Final value.
#endif
       ----------------------------------------

-- Create the default scheduler(s) state:
makeScheds :: Int -> IO [Sched]
makeScheds main = do
   when dbg$ do tid <- myThreadId
                printf "[initialization] Creating %d worker threads, currently on %s\n" numCapabilities (show tid)
   workpools <- replicateM numCapabilities $ R.newQ
   rngs      <- replicateM numCapabilities $ Random.create >>= newHotVar
   idle      <- newHotVar []
   -- The STACKs are per-worker.. but the root finished flag is shared between all anonymous system workers:
   sessionFinished <- newHotVar False
   sessionStacks   <- mapM newHotVar (replicate numCapabilities [Session baseSessionID sessionFinished])
   activeSessions  <- newHotVar S.empty
   sessionCounter  <- newHotVar (baseSessionID + 1)
   let allscheds = [ Sched { no=x, idle, isMain= (x==main),
                             workpool=wp, scheds=allscheds, rng=rng,
                             sessions = stck,
                             activeSessions=activeSessions,
                             sessionCounter=sessionCounter
                           }
                   --  | (x,wp,rng,stck) <- zip4 [0..] workpools rngs sessionStacks
                   | x   <- [0 .. numCapabilities-1]
                   | wp  <- workpools
                   | rng <- rngs
                   | stck <- sessionStacks
                   ]
   return allscheds


-- The ID of top-level runPar sessions.
baseSessionID :: SessionID
baseSessionID = 1000


--------------------------------------------------------------------------------
-- IVar operations
--------------------------------------------------------------------------------

{-# INLINE new  #-}
-- | Creates a new @IVar@
new :: Par (IVar a)
new  = io$ do r <- newIORef Empty
              return (IVar r)

{-# INLINE get  #-}
-- | Read the value in an @IVar@.  The 'get' operation can only return when the
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
#  ifdef DEBUG_DIRECT
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
#ifdef DEBUG_DIRECT
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
#ifdef DEBUG_DIRECT
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
       do _ <- spawn_$ pMap kont rest
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
     do _ <- spawn_$ io$ kont arg
        pMap more rest

   -- parchain [kont] = kont arg
   -- parchain (kont:rest) = do spawn$ io$ kont arg
   --                           parchain rest


------------------------------------------------------------
{-# INLINE fork #-}
fork :: Par () -> Par ()
fork task =
  -- Forking the "parent" means offering up the continuation of the
  -- fork rather than the task argument for stealing:
  case _FORKPARENT of
    True -> do
      sched <- RD.ask
      callCC$ \parent -> do
         let wrapped = parent ()
         io$ pushWork sched wrapped
         -- Then execute the child task and return to the scheduler when it is complete:
         task
         -- If we get to this point we have finished the child task:
         _ <- longjmpSched -- We reschedule to pop the cont we pushed.
         -- TODO... OPTIMIZATION: we could also try the pop directly, and if it succeeds return normally....
         io$ printf " !!! ERROR: Should never reach this point #1\n"

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
longjmpSched = Par $ C.ContT (\ _k -> rescheduleR 0 (trivialCont "longjmpSched"))

-- Reschedule the scheduler loop until it observes sessionFinished==True, and
-- then it finally invokes its continuation.
rescheduleR :: Word64 -> (a -> ROnly ()) -> ROnly ()
rescheduleR cnt kont = do
  mysched <- RD.ask
  when dbg$ liftIO$ do tid <- myThreadId
                       sess <- readSessions mysched
                       null <- R.nullQ (workpool mysched)
                       printf " [%d %s]  - Reschedule #%d... sessions %s, pool empty %s\n"
                              (no mysched) (show tid) cnt (show sess) (show null)
  mtask  <- liftIO$ popWork mysched
  case mtask of
    Nothing -> do
                  (Session _ finRef):_ <- liftIO$ readIORef $ sessions mysched
                  fin <- liftIO$ readIORef finRef
                  if fin
                   then do when (dbglvl >= 1) $ liftIO $ do
                             tid <- myThreadId
                             sess <- readSessions mysched
                             printf " [%d %s]  - DROP out of reschedule loop, sessionFinished=%s, all sessions %s\n"
                                    (no mysched) (show tid) (show fin) (show sess)
                             empt <- R.nullQ$ workpool mysched
                             when (not empt) $ do
                               printf " [%d %s] - WARNING - leaving rescheduleR while local workpoll is nonempty\n"
                                      (no mysched) (show tid)

                           kont (error "Direct.hs: The result value from rescheduleR should not be used.")
                   else do
                     -- when (dbglvl >= 1) $ liftIO $ do
                     --     tid <- myThreadId
                     --     sess <- readSessions mysched
                     --     printf " [%d %s]  -    Apparently NOT finished with head session... trying to steal, all sessions %s\n"
                     --            (no mysched) (show tid) (show sess)
                     liftIO$ steal mysched
#ifdef WAKEIDLE
--                     io$ tryWakeIdle (idle mysched)
#endif
                     liftIO yield
                     rescheduleR (cnt+1) kont
    Just task -> do
       -- When popping work from our own queue the Sched (Reader value) stays the same:
       when dbg $ do sn <- liftIO$ makeStableName task
                     liftIO$ printf " [%d] popped work %d from own queue\n" (no mysched) (hashStableName sn)
       let C.ContT fn = unPar task
       -- Run the stolen task with a continuation that returns to the scheduler if the task exits normally:
       fn (\ _ -> do
           sch <- RD.ask
           when dbg$ liftIO$ printf "  + task finished successfully on cpu %d, calling reschedule continuation..\n" (no sch)
           rescheduleR 0 kont)


-- | Attempt to steal work or, failing that, give up and go idle.
--
--   The current policy is to do a burst of of N tries without
--   yielding or pausing in between.
steal :: Sched -> IO ()
steal mysched@Sched{ idle, scheds, rng, no=my_no } = do
  when (dbglvl>=2)$ do tid <- myThreadId
                       printf " [%d %s]  + stealing\n" my_no (show tid)
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
                     when dbg$ printf " [%d]  | waking up all threads\n" my_no
                     writeHotVarRaw idle []
                     mapM_ (\vr -> putMVar vr True) r
                  else do
                    (Session _ finRef):_ <- readIORef $ sessions mysched
                    fin <- readIORef finRef
                    done <- if fin then pure True else takeMVar m
                    if done
                       then do
                         when dbg$ printf " [%d]  | shutting down\n" my_no
                         return ()
                       else do
                         when dbg$ printf " [%d]  | woken up\n" my_no
                         i <- getnext (-1::Int)
                         go maxtries i

    -- We need to return from this loop to check sessionFinished and exit the scheduler if necessary.
    go 0 _i | _IDLING_ON == False = yield

    ----------------------------------------
    go tries i
      | i == my_no = do i' <- getnext i
                        go (tries-1) i'

      | otherwise     = do
         -- We ONLY go through the global sched array to access victims:
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
_errK :: t
_errK = error "Error cont: this closure shouldn't be used"

trivialCont :: String -> a -> ROnly ()
#ifdef DEBUG_DIRECT
trivialCont str _ = do
--                trace (str ++" trivialCont evaluated!")
                liftIO$ printf " !! trivialCont evaluated, msg: %s\n" str
#else
trivialCont _str _ = do
#endif
                return ()

----------------------------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- <boilerplate>

-- TEMP: TODO: Factor out this boilerplate somehow.

{-# INLINE spawn1_ #-}
-- Spawn a one argument function instead of a thunk.  This is good for debugging if the value supports "Show".
spawn1_ f x =
#ifdef DEBUG_DIRECT
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
#ifdef DEBUG_DIRECT
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



#ifdef NEW_GENERIC
instance PU.ParMonad Par where
  fork = fork
  internalLiftIO io = Par (lift $ lift io)

instance PU.ParThreadSafe Par where
  unsafeParIO io = Par (lift $ lift io)

instance PN.ParFuture Par where
  type Future Par = IVar
  type FutContents Par a = ()
  get    = get
  spawn  = spawn
  spawn_ = spawn_
  spawnP = spawnP

instance PN.ParIVar Par  where
  new  = new
  put_ = put_
  newFull = newFull
  newFull_ = newFull_
#endif

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
_sanityCheck :: [Sched] -> IO ()
_sanityCheck allscheds = do
  forM_ allscheds $ \ Sched{no, workpool} -> do
     b <- R.nullQ workpool
     when (not b) $ do
         () <- printf "WARNING: After main thread exited non-empty queue remains for worker %d\n" no
         return ()
  printf "Sanity check complete.\n"


-- | This tries to localize the blocked-indefinitely exception:
_dbgTakeMVar :: String -> MVar a -> IO a
_dbgTakeMVar msg mv =
--  catch (takeMVar mv) ((\_ -> doDebugStuff) :: BlockedIndefinitelyOnMVar -> IO a)
  E.catch (takeMVar mv) (\(_::IOError) -> doDebugStuff)
 where
   doDebugStuff = do printf "This takeMVar blocked indefinitely!: %s\n" msg
                     error "failed"

-- | For debugging purposes.  This can help us figure out (by an ugly
--   process of elimination) which MVar reads are leading to a "Thread
--   blocked indefinitely" exception.
{-
busyTakeMVar :: String -> MVar a -> IO a
busyTakeMVar msg mv = try (10 * 1000 * 1000)
 where
 try 0 = do
   when dbg $ do
     tid <- myThreadId
     -- After we've failed enough times, start complaining:
     printf "%s not getting anywhere, msg: %s\n" (show tid) msg
   try (100 * 1000)
 try n = do
   x <- tryTakeMVar mv
   case x of
     Just y  -> return y
     Nothing -> do yield; try (n-1)
-}

-- | Fork a thread but ALSO set up an error handler that suppresses
--   MVar exceptions.
_forkIO_Suppress :: Int -> IO () -> IO ThreadId
_forkIO_Suppress whre action =
  forkOn whre $
           E.handle (\e ->
                      case (e :: E.BlockedIndefinitelyOnMVar) of
                       _ -> do
                               putStrLn$"CAUGHT child thread exception: "++show e
                               return ()
                    )
           action


-- | Exceptions that walk up the fork tree of threads:
forkWithExceptions :: (IO () -> IO ThreadId) -> String -> IO () -> IO ThreadId
forkWithExceptions forkit descr action = do
   parent <- myThreadId
   forkit $ do
      tid <- myThreadId
      E.catch action
         (\ e ->
           case E.fromException e of
             Just E.ThreadKilled -> printf
                                    "\nThreadKilled exception inside child thread, %s (not propagating!): %s\n" (show tid) (show descr)
             _  -> do printf
                        "\nException inside child thread %s, %s: %s\n" (show descr) (show tid) (show e)
                      E.throwTo parent (e :: E.SomeException)
         )


-- Do all the memory reads to snapshot the current session stack:
readSessions :: Sched -> IO [(SessionID, Bool)]
readSessions sched = do
  ls <- readIORef (sessions sched)
  bools <- mapM (\ (Session _ r) -> readIORef r) ls
  return (zip (map (\ (Session sid _) -> sid) ls) bools)
