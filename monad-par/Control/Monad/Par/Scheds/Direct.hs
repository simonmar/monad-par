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
   Sched(..), Par,
   IVar(..), IVarContents(..),
--    sched,
    runPar, 
    new, get, put_, fork,
    newFull, newFull_, put,
    spawn, spawn_, spawnP,
    spawn1_
--   runParAsync, runParAsyncHelper,
--   yield,
 ) where

import Control.Applicative
import Control.Concurrent hiding (yield)
import Debug.Trace
import Data.IORef
import Text.Printf
import GHC.Conc
import "mtl" Control.Monad.Cont as C
import qualified "mtl" Control.Monad.Reader as RD
-- import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Sequence as Seq
import System.Random.MWC as Random
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName
import qualified Control.Monad.Par.Class  as PC
import qualified Control.Monad.Par.Unsafe as UN
import qualified Data.Map as M
import Control.DeepSeq (NFData(rnf), deepseq)

-- import Data.Concurrent.Deque.Class as DQ
#ifdef REACTOR_DEQUE
-- These performed ABYSMALLY:
import Data.Concurrent.Deque.ChaseLev
import Data.Concurrent.Deque.ChaseLev.DequeInstance
import qualified Data.Concurrent.Deque.ReactorDeque as R
import Data.Array.IO
#else
import Data.Concurrent.Deque.Class (WSDeque)
import Data.Concurrent.Deque.Reference.DequeInstance
import Data.Concurrent.Deque.Reference as R
#endif

import Control.Exception(fromException, handle, BlockedIndefinitelyOnMVar, SomeException, IOException, catch)

import Prelude hiding (null, catch)
import qualified Prelude

--------------------------------------------------------------------------------
-- Configuration Toggles
--------------------------------------------------------------------------------

-- define DEBUG
-- [2012.08.30] This shows a 10X improvement on nested parfib:
#define NESTED_SCHEDS
#define PARPUTS
-- define FORKPARENT
-- define IDLING_ON
   -- Next, IF idling is on, should we do wakeups?:
#define WAKEIDLE

-------------------------------------------------------------------
-- Ifdefs for the above preprocessor defines.  Try to MINIMIZE code
-- that lives in this dangerous region:
--------------------------------------------------------------------

#ifdef DEBUG
import System.Environment (getEnvironment)
theEnv = unsafePerformIO $ getEnvironment
dbg = True
dbglvl = 1
#else
dbg = False
dbglvl = 0
#endif

#ifdef PARPUTS
_PARPUTS = True
#else
_PARPUTS = False
#endif

#ifdef FORKPARENT
_FORKPARENT = True
#else
#warning "FORKPARENT POLICY NOT USED; THIS IS GENERALLY WORSE"
_FORKPARENT = False
#endif

#ifdef IDLING_ON
_IDLING_ON = True
#else
_IDLING_ON = False
#endif

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
    deriving (Monad, MonadCont, RD.MonadReader Sched)
type ROnly = RD.ReaderT Sched IO

data Sched = Sched 
    { 
      ---- Per worker ----
      no       :: {-# UNPACK #-} !Int,
#ifdef REACTOR_DEQUE
      workpool :: R.Deque IOArray (Par ()),
#else
      workpool :: WSDeque (Par ()),
#endif
      rng      :: HotVar GenIO, -- Random number gen for work stealing.
      isMain :: Bool, -- Are we the main/master thread? 

      ---- Global data: ----
      killflag :: HotVar Bool,
      idle     :: HotVar [MVar Bool],
--      scheds   :: [Sched],   -- A global list of schedulers.
      -- A pointer to the global vector of schedulers.  It changes to /grow/.
      schedPool :: HotVar (MV.IOVector Sched),

      ------ Nested Support -------

      -- In the nested scheme, each sched is working only at a SINGLE
      -- pedigree.  The contract is that the workpool associated with
      -- this sched may only ever produce work of this pedigree.  Thus
      -- nested scheduling avoids forking extra threads, but it does
      -- not avoid creating a new Sched for a deeper runPar session.
      pedigree :: Pedigree,

      -- For nested support, our scheduler may be working on behalf of
      -- a REAL haskell continuation that we need to return to.  In
      -- that case we need to know WHEN to stop rescheduling and
      -- return to that genuine continuation.
      nestedFinished :: Maybe (HotVar Bool)
     }

newtype IVar a = IVar (IORef (IVarContents a))

data IVarContents a = Full a | Empty | Blocked [a -> IO ()]

unsafeParIO :: IO a -> Par a 
unsafeParIO io = Par (lift$ lift io)
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

{-# INLINE amINested #-}
{-# INLINE registerWorker #-}
{-# INLINE unregisterWorker #-}
amINested :: ThreadId -> IO (Maybe Sched)
registerWorker :: ThreadId -> Sched -> IO ()
unregisterWorker :: ThreadId -> IO ()
#ifdef NESTED_SCHEDS
amINested tid = do
  -- There is no race here.  Each thread inserts itself before it
  -- becomes an active worker.
  wp <- readIORef globalWorkerPool
  case M.lookup tid wp of
    Nothing -> return Nothing
    Just Sched{nestedFinished=Nothing, no} -> do
      when (dbglvl>=0) $ printf " [%d] SHALLOW strategy: NOT nesting, because we're already nested.\n" no
      return Nothing
    oth -> return oth
registerWorker tid sched = 
  atomicModifyIORef globalWorkerPool $ 
    \ mp -> (M.insert tid sched mp, ())
unregisterWorker tid = 
  atomicModifyIORef globalWorkerPool $ 
    \ mp -> (M.delete tid mp, ())
#else 
amINested      _     = return Nothing
registerWorker _ _   = return ()
unregisterWorker tid = return ()
#endif

--------------------------------------------------------------------------------
-- Helpers #0: Pedigrees
--------------------------------------------------------------------------------

-- | The Pedigree distinguishes where a runPar instance is in the tree of
-- nested runPar instances.  This type can be just a counter, or it
-- can include a list/set of "parent" runPar ids.
type Pedigree = Int

-- | Is it safe help out by stealing work of a given pedigree, or does
--   it raise the potential for deadlock?
canWork :: Pedigree -> Pedigree -> Bool
canWork myID     -- ^ Our pedigree
        targetID -- ^ Potential target pedigree
  = targetID >= myID

-- | The Pedigree of the very first "runPar".
rootPedigree :: Pedigree
rootPedigree = 0

-- | Extend a pedigree to get a child pedigree.
extendPedigree :: Pedigree -> Pedigree
extendPedigree p = p + 1


--------------------------------------------------------------------------------
-- Helpers #1:  Atomic Variables
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
-- Helpers #2:  Pushing and popping work.
-----------------------------------------------------------------------------

{-# INLINE popWork  #-}
popWork :: Sched -> IO (Maybe (Par ()))
popWork Sched{ workpool, no } = do 
  mb <- R.tryPopL workpool 
  when dbg $ case mb of 
         Nothing -> return ()
	 Just x  -> do sn <- makeStableName mb
		       printf " [%d]                                   -> POP work unit %d\n" no (hashStableName sn)
  return mb

{-# INLINE pushWork #-}
pushWork :: Sched -> Par () -> IO ()
pushWork Sched { workpool, idle, no, isMain } task = do
--  modifyHotVar_ workpool (`pushL` task)
  R.pushL workpool task
  when dbg $ do sn <- makeStableName task
		printf " [%d]                                   -> PUSH work unit %d\n" no (hashStableName sn)
#if  defined(IDLING_ON) && defined(WAKEIDLE)
  --when isMain$    -- Experimenting with reducing contention by doing this only from a single thread.
                    -- TODO: We need to have a proper binary wakeup-tree.
  tryWakeIdle idle
#endif
  return ()

tryWakeIdle idle = do
-- NOTE: I worry about having the idle var hammmered by all threads on their spawn-path:
  -- If any worker is idle, wake one up and give it work to do.
  idles <- readHotVar idle -- Optimistically do a normal read first.
  when (not (Prelude.null idles)) $ do
    when dbg$ printf "Waking %d idle thread(s).\n" (length idles)
    r <- modifyHotVar idle (\is -> case is of
                             []     -> ([], return ())
                             (i:is) -> (is, putMVar i False))
    r -- wake an idle worker up by putting an MVar.

rand :: HotVar GenIO -> IO Int
rand ref = Random.uniformR (0, numCapabilities-1) =<< readHotVar ref

--------------------------------------------------------------------------------
-- Running computations in the Par monad
--------------------------------------------------------------------------------

instance NFData (IVar a) where
  rnf _ = ()

runPar userComp = unsafePerformIO $ do
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
     Just sched -> runNestedPar sched tid userComp
     Nothing -> do
       allscheds <- makeScheds main_cpu
       m <- newEmptyMVar
       forM_ [0 .. (MV.length allscheds)] $ \cpu -> do
            sched <- MV.read allscheds cpu 
            forkOn cpu $ do 
              tid <- myThreadId
              registerWorker tid sched
              if (cpu /= main_cpu)
                 then do when dbg$ printf " [%d] Entering scheduling loop.\n" cpu
                         runReaderWith sched $ rescheduleR errK
                         when dbg$ printf " [%d] Exited scheduling loop.  FINISHED.\n" cpu
                         return ()
                 else do
                      let userComp'  = do when dbg$ io$ printf " [%d] Starting Par computation on main thread.\n" main_cpu
                                          res <- userComp
                                          finalSched <- RD.ask 
                                          when dbg$ io$ printf " [%d] Out of Par computation on main thread.  Writing MVar...\n" (no finalSched)

                                          -- Sanity check our work queues:
--                                          when dbg $ io$ sanityCheck (MV.toList allscheds)
                                          io$ putMVar m res

                      RD.runReaderT (C.runContT (unPar userComp') trivialCont) sched
--                      when dbg$ do putStrLn " *** Out of entire runContT user computation on main thread."
--                                   sanityCheck (MV.toList allscheds)
                      -- Not currently requiring that other scheduler threads have exited before we 
                      -- (the main thread) exit.  But we do signal here that they should terminate:
                      writeIORef (killflag sched) True
              unregisterWorker tid 

       when dbg$ do putStrLn " *** Reading final MVar on main thread."   
       -- We don't directly use the thread we come in on.  Rather, that thread waits
       -- waits.  One reason for this is that the main/progenitor thread in
       -- GHC is expensive like a forkOS thread.
       takeMVar m -- Final value.
--       dbgTakeMVar "global waiting thread" m -- Final value.
--       busyTakeMVar " global wait " m -- Final value.                    


{-# INLINE runNestedPar #-}
runNestedPar :: Sched -> ThreadId -> Par a -> IO a
runNestedPar (sched@Sched{pedigree,schedPool}) tid userComp =
 do 
    when (dbglvl>=1)$ printf " [%d %s] Engaging embedding nested work....\n" (no sched) (show tid)
    -- Here the current thread is ALREADY a worker.  We want to
    -- plug the new computation into the current pool of workers.
    -- HOWEVER, this is a complicated business due to the potential
    -- for deadlock.

    -- FIRST: we must create a new scheduler with a distinct pedigree
    -----------------------------------------------------------------
    -- Here we have an extra IORef to communicate the answer... ugly.
    ref <- newIORef (error "this should never be touched")
    newBool <- newHotVar False
    let freshSched = sched { pedigree = extendPedigree pedigree, 
                             nestedFinished= Just newBool }

    -- NEXT: there are THREE options for HOW to schedule the nested work
    --------------------------------------------------------------------
    -- How these are combined constitutes a significant design choice.
        -- (OPTION 1) - IF the current Sched has run dry, we can put ourselves in its place.
        replaceCurrent = do
            pool <- readHotVar schedPool
            MV.write pool (no sched) freshSched
            error "unfinished... write me"
        -- (OPTION 2) - We can WAIT, doing work until the current Sched runs dry and OPTION 1 applies.
        waitForParent  = do
            isdry <- nullQ (workpool sched)
            if isdry then return () else do 
--               rescheduleR -- Do one work item.
--               RD.runReaderT (runOne sched) freshSched
               runOne sched
               waitForParent

        -- (OPTION 3) - We can INCREASE the number of active steal targets, replacing the global array.
        addExtra       = error "Unimplemented- the problem is we don't have a good way to shrink"


        cont ans = liftIO$ do writeIORef ref ans; writeHotVarRaw newBool True
        -- When we're ready, this actually runs 
        engageNested = do RD.runReaderT (C.runContT (unPar userComp) cont) freshSched
                          when (dbglvl>=1)$ printf " [%d %s] RETURN from nested runContT\n" (no sched) (show tid)

    -- Here's our current policy:
    waitForParent
    replaceCurrent
    engageNested
        
    -- By returning here we ARE reengaging the scheduler, since we
    -- are ALREADY inside the rescheduleR loop on this thread:
    readIORef ref  -- The answer.

-- | In contrast with rescheduleR, this runs a single work item (if available) and returns.
runOne mysched = RD.runReaderT comp mysched
 where
  comp = do
   mtask  <- liftIO$ popWork mysched
   case mtask of
     Nothing -> return ()
     Just task -> do
        -- When popping work from our own queue the Sched (Reader value) stays the same:
        let C.ContT fn = unPar task 
        -- Run the stolen task with a continuation that returns to the scheduler if the task exits normally:
        fn (\ () -> return () )
        return ()


-- Make sure there is no work left in any deque after exiting.
sanityCheck :: [Sched] -> IO ()
sanityCheck allscheds = do
  forM_ allscheds $ \ Sched{no, workpool} -> do
     b <- R.nullQ workpool
     when (not b) $ do 
         printf "WARNING: After main thread exited non-empty queue remains for worker %d\n" no
         return ()
  putStrLn "Sanity check complete."


-- Create the default scheduler(s) state:
makeScheds :: Int -> IO (MV.IOVector Sched)
makeScheds main = do
   when dbg$ printf "[initialization] Creating %d worker threads\n" numCapabilities
   workpools <- replicateM numCapabilities $ R.newQ
   rngs      <- replicateM numCapabilities $ Random.create >>= newHotVar 
   idle      <- newHotVar []   
   killflag  <- newHotVar False
   empty     <- MV.new 0
   schedPool <- newHotVar empty -- (V.thaw V.empty)
   allscheds <- V.thaw $ V.fromList
                   [ Sched { no=x, idle, killflag, isMain= (x==main),
			     workpool=wp, rng=rng,
                             schedPool= schedPool,
                             pedigree= rootPedigree,
                             nestedFinished= Nothing
			   }
                   | (x,wp,rng) <- zip3 [0..] workpools rngs]
   writeHotVarRaw schedPool allscheds
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
get iv@(IVar v) =  do 
  callCC $ \cont -> 
    do
       e  <- io$ readIORef v
       case e of
	  Full a -> return a
	  _ -> do
            sch <- RD.ask
#  ifdef DEBUG
            sn <- io$ makeStableName iv
            let resched = trace (" ["++ show (no sch) ++ "]  - Rescheduling on unavailable ivar "++show (hashStableName sn)++"!") 
#else
            let resched = 
#  endif
			  reschedule
            -- Because we continue on the same processor the Sched stays the same:
            -- TODO: Try NOT using monads as first class values here.  Check for performance effect:
	    r <- io$ atomicModifyIORef v $ \e -> case e of
		      Empty      -> (Blocked [pushWork sch . cont], resched)
		      Full a     -> (Full a, return a)
		      Blocked ks -> (Blocked (pushWork sch . cont:ks), resched)
	    r

-- | NOTE unsafePeek is NOT exposed directly through this module.  (So
-- this module remains SAFE in the Safe Haskell sense.)  It can only
-- be accessed by importing Control.Monad.Par.Unsafe.
{-# INLINE unsafePeek #-}
unsafePeek iv@(IVar v) = do 
  e  <- io$ readIORef v
  case e of 
    Full a -> return (Just a)
    _      -> return Nothing

------------------------------------------------------------
{-# INLINE put_ #-}
-- | @put_@ is a version of @put@ that is head-strict rather than fully-strict.
--   In this scheduler, puts immediately execute woken work in the current thread.
put_ iv@(IVar v) !content = do
   sched <- RD.ask
   ks <- io$ do 
      ks <- atomicModifyIORef v $ \e -> case e of
               Empty      -> (Full content, [])
               Full _     -> error "multiple put"
               Blocked ks -> (Full content, ks)
#ifdef DEBUG
      when (dbglvl >=  3) $ do 
         sn <- makeStableName iv
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
unsafeTryPut iv@(IVar v) !content = do
   -- Head strict rather than fully strict.
   sched <- RD.ask 
   (ks,res) <- io$ do 
      pr@(ks,res) <- atomicModifyIORef v $ \e -> case e of
		   Empty      -> (Full content, ([], content))
		   Full x     -> (Full x, ([], x))
		   Blocked ks -> (Full content, (ks, content))
#ifdef DEBUG
      sn <- makeStableName iv
      printf " [%d] unsafeTryPut: value %s in IVar %d.  Waking up %d continuations.\n" 
	     (no sched) (show content) (hashStableName sn) (length ks)
#endif
      return pr
   wakeUp sched ks content
   return res

-- | When an IVar is filled in, continuations wake up.
{-# INLINE wakeUp #-}
wakeUp :: Sched -> [a -> IO ()]-> a -> Par ()
wakeUp sched ks arg = loop ks
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
         -- Is it possible to slip in a new Sched here?
         -- let wrapped = lift$ RD.runReaderT (parent ()) undefined
         io$ pushWork sched wrapped
         -- Then execute the child task and return to the scheduler when it is complete:
         task 
         -- If we get to this point we have finished the child task:
         reschedule -- We reschedule to pop the cont we pushed.
         io$ putStrLn " !!! ERROR: Should not reach this point #1"   

      when dbg$ do 
       sched2 <- RD.ask 
       io$ printf "     called parent continuation... was on cpu %d now on cpu %d\n" (no sched) (no sched2)
       return ()

    False -> do 
      sch <- RD.ask
      when dbg$ io$ printf " [%d] forking task...\n" (no sch)
      io$ pushWork sch task
   
-- This routine "longjmp"s to the scheduler, throwing out its own continuation.
reschedule :: Par a 
reschedule = Par $ C.ContT rescheduleR


-- Reschedule ignores its continuation.
-- It runs the scheduler loop indefinitely, until it observers killflag==True
rescheduleR :: ignoredCont -> ROnly ()
rescheduleR k = do
  mysched <- RD.ask

-- rescheduleIO :: Sched -> ignoredCont -> IO ()
  
  when dbg$ liftIO$ printf " [%d]  - Reschedule...\n" (no mysched)
  mtask  <- liftIO$ popWork mysched
  case mtask of
    Nothing -> do 
                  k <- liftIO$ readIORef (killflag mysched)
                  fin <- liftIO$ case nestedFinished mysched of
                                   Nothing -> return False
                                   Just r  -> readIORef r
		  if (k || fin) 
                   then do when (dbglvl>=0) $ 
                             liftIO$ printf " [%d]  - DROP out of reschedule loop, killflag=%s, nestedFinished=%s\n"
                                            (no mysched) (show k) (show fin)
                           return ()
                   else do
		     liftIO$ steal mysched
#ifdef WAKEIDLE
--                     io$ tryWakeIdle (idle mysched)
#endif
		     rescheduleR errK
    Just task -> do
       -- When popping work from our own queue the Sched (Reader value) stays the same:
       when dbg $ do sn <- liftIO$ makeStableName task
		     liftIO$ printf " [%d] popped work %d from own queue\n" (no mysched) (hashStableName sn)
       let C.ContT fn = unPar task 
       -- Run the stolen task with a continuation that returns to the scheduler if the task exits normally:
       fn (\ () -> do 
           sch <- RD.ask
           when dbg$ liftIO$ printf "  + task finished successfully on cpu %d, calling reschedule continuation..\n" (no sch)
	   rescheduleR errK)

{-# INLINE runReaderWith #-}
runReaderWith state m = RD.runReaderT m state


-- | Attempt to steal work or, failing that, give up and go idle.
steal :: Sched -> IO ()
steal mysched@Sched{ idle, rng, no=my_no, schedPool } = do
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

    -- We need to return from this loop to check killflag and exit the scheduler if necessary.
    go 0 i | _IDLING_ON == False = yield

    ----------------------------------------
    go tries i
      | i == my_no = do i' <- getnext i
			go (tries-1) i'

      | otherwise     = do
         allscheds <- readHotVar schedPool -- It's ok if this changes out from under us.    
         victim <- MV.read allscheds  i    -- The worst that happens is we miss out on the latest work.

         if canWork (pedigree mysched) (pedigree victim) then do           
            when (dbglvl>=2)$ printf " [%d]  | trying steal from %d\n" my_no (no victim)
            r <- R.tryPopR (workpool victim)
            case r of
              Just task  -> do
                 when dbg$ do sn <- makeStableName task
                              printf " [%d]  | stole work (unit %d) from cpu %d\n" my_no (hashStableName sn) (no victim)
                 runReaderWith mysched $ 
                   C.runContT (unPar task)
                    (\_ -> do
                      when dbg$ do sn <- liftIO$ makeStableName task
                                   liftIO$ printf " [%d]  | DONE running stolen work (unit %d) from %d\n" my_no (hashStableName sn) (no victim)
                      return ())
              Nothing -> do i' <- getnext i
                            go (tries-1) i'
          else do 
            error "FINISH ME -- can't steal pedigree"
         
errK = error "this closure shouldn't be used"

trivialCont :: a -> ROnly ()
trivialCont _ = 
#ifdef DEBUG
                trace "trivialCont evaluated!"
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


-- DEBUGGING TOOLs
--------------------------------------------------------------------------------

dbgTakeMVar :: String -> MVar a -> IO a
dbgTakeMVar msg mv = 
--  catch (takeMVar mv) ((\_ -> doDebugStuff) :: BlockedIndefinitelyOnMVar -> IO a)
  catch (takeMVar mv) ((\_ -> doDebugStuff) :: IOError -> IO a)
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
--   B.putStrLn (B.pack$ show tid ++ "! ")
   putStrLn (show tid ++" not getting anywhere, msg: "++ msg)  -- After we've failed enough times, start complaining.
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
           handle (\e -> 
--                   case fromException (e::SomeException) :: Maybe IOException of
                    case (e::BlockedIndefinitelyOnMVar) of
                     _ -> do 
                             putStrLn$"CAUGHT child thread exception: "++show e 
                             return ()
		  )
           action
