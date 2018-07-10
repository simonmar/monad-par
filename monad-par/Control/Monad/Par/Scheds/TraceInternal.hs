{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- | This module exposes the internals of the @Par@ monad so that you
-- can build your own scheduler or other extensions.  Do not use this
-- module for purposes other than extending the @Par@ monad with new
-- functionality.

module Control.Monad.Par.Scheds.TraceInternal (
   Trace(..), Sched(..), Par(..),
   IVar(..), IVarContents(..),
   sched,
   runPar, runParIO, runParAsync,
   -- runParAsyncHelper,
   new, newFull, newFull_, get, put_, put,
   pollIVar, yield, fixPar, FixParException (..)
 ) where


import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import Data.IORef
import System.IO.Unsafe
#if MIN_VERSION_base(4,4,0)
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
#else
import GHC.IO.Unsafe (unsafeInterleaveIO)
#endif
import Control.Concurrent hiding (yield)
import GHC.Conc (numCapabilities)
import Control.DeepSeq
import Control.Monad.Fix (MonadFix (mfix))
import Control.Exception (Exception, throwIO, BlockedIndefinitelyOnMVar (..),
                          catch)
-- import Text.Printf

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

#if __GLASGOW_HASKELL__ <= 700
import GHC.Conc (forkOnIO)
forkOn = forkOnIO
#endif


-- ---------------------------------------------------------------------------

data Trace = forall a . Get (IVar a) (a -> Trace)
           | forall a . Put (IVar a) a Trace
           | forall a . New (IVarContents a) (IVar a -> Trace)
           | Fork Trace Trace
           | Done
           | Yield Trace
           | forall a . LiftIO (IO a) (a -> Trace)

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
-- threads work-queue because it can be stolen by other threads.
--       else return ()

    Yield parent -> do
        -- Go to the end of the worklist:
        let Sched { workpool } = queue
        -- TODO: Perhaps consider Data.Seq here.
        -- This would also be a chance to steal and work from opposite ends of the queue.
        atomicModifyIORef workpool $ \ts -> (ts++[parent], ())
        reschedule queue
    LiftIO io c -> do
        r <- io
        loop (c r)

data FixParException = FixParException deriving Show
instance Exception FixParException

-- | Process the next item on the work queue or, failing that, go into
--   work-stealing mode.
reschedule :: Sched -> IO ()
reschedule queue@Sched{ workpool } = do
  e <- atomicModifyIORef workpool $ \ts ->
         case ts of
           []      -> ([], Nothing)
           (t:ts') -> (ts', Just t)
  case e of
    Nothing -> steal queue
    Just t  -> sched True queue t


-- RRN: Note -- NOT doing random work stealing breaks the traditional
-- Cilk time/space bounds if one is running strictly nested (series
-- parallel) programs.

-- | Attempt to steal work or, failing that, give up and go idle.
steal :: Sched -> IO ()
steal q@Sched{ idle, scheds, no=my_no } = do
  -- printf "cpu %d stealing\n" my_no
  go scheds
  where
    go [] = do m <- newEmptyMVar
               r <- atomicModifyIORef idle $ \is -> (m:is, is)
               if length r == numCapabilities - 1
                  then do
                     -- printf "cpu %d initiating shutdown\n" my_no
                     mapM_ (\m -> putMVar m True) r
                  else do
                    done <- takeMVar m
                    if done
                       then do
                         -- printf "cpu %d shutting down\n" my_no
                         return ()
                       else do
                         -- printf "cpu %d woken up\n" my_no
                         go scheds
    go (x:xs)
      | no x == my_no = go xs
      | otherwise     = do
         r <- atomicModifyIORef (workpool x) $ \ ts ->
                 case ts of
                    []     -> ([], Nothing)
                    (x:xs) -> (xs, Just x)
         case r of
           Just t  -> do
              -- printf "cpu %d got work from cpu %d\n" my_no (no x)
              sched True q t
           Nothing -> go xs

-- | If any worker is idle, wake one up and give it work to do.
pushWork :: Sched -> Trace -> IO ()
pushWork Sched { workpool, idle } t = do
  atomicModifyIORef workpool $ \ts -> (t:ts, ())
  idles <- readIORef idle
  when (not (null idles)) $ do
    r <- atomicModifyIORef idle (\is -> case is of
                                          [] -> ([], return ())
                                          (i:is) -> (is, putMVar i False))
    r -- wake one up

data Sched = Sched
    { no       :: {-# UNPACK #-} !Int,
      workpool :: IORef [Trace],
      idle     :: IORef [MVar Bool],
      scheds   :: [Sched] -- Global list of all per-thread workers.
    }
--  deriving Show

newtype Par a = Par {
    runCont :: (a -> Trace) -> Trace
}

instance Functor Par where
    fmap f m = Par $ \c -> runCont m (c . f)

instance Monad Par where
    return = pure
    m >>= k  = Par $ \c -> runCont m $ \a -> runCont (k a) c

instance Applicative Par where
   (<*>) = ap
   pure a = Par ($ a)

instance MonadFix Par where
   mfix = fixPar

-- | Take the monadic fixpoint of a 'Par' computation. This is
-- the definition of 'mfix' for 'Par'. Throws 'FixParException'
-- if the result is demanded strictly within the computation.
fixPar :: (a -> Par a) -> Par a
-- We do this IO-style, rather than ST-style, in order to get a
-- consistent exception type. Using the ST-style mfix, a strict
-- argument could lead us to *either* a <<loop>> exception *or*
-- (if the wrong sort of computation gets re-run) a "multiple-put"
-- error.
fixPar f = Par $ \ c ->
  LiftIO (do
    mv <- newEmptyMVar
    ans <- unsafeDupableInterleaveIO (readMVar mv
             `catch` \ ~BlockedIndefinitelyOnMVar -> throwIO FixParException)
    case f ans of
      Par q -> pure $ q $ \a -> LiftIO (putMVar mv a) (\ ~() -> c a)) id

#if !MIN_VERSION_base(4,4,0)
unsafeDupableInterleaveIO :: IO a -> IO a
unsafeDupableInterleaveIO = unsafeInterleaveIO
#endif

newtype IVar a = IVar (IORef (IVarContents a))
-- data IVar a = IVar (IORef (IVarContents a))

-- | Equality for IVars is physical equality, as with other reference types.
instance Eq (IVar a) where
  (IVar r1) == (IVar r2) = r1 == r2

instance NFData (IVar a) where
  rnf !_ = ()


-- From outside the Par computation we can peek.  But this is nondeterministic.
pollIVar :: IVar a -> IO (Maybe a)
pollIVar (IVar ref) =
  do contents <- readIORef ref
     case contents of
       Full x -> return (Just x)
       _      -> return (Nothing)


data IVarContents a = Full a | Empty | Blocked [a -> Trace]


{-# INLINE runPar_internal #-}
runPar_internal :: Bool -> Par a -> IO a
runPar_internal _doSync x = do
   workpools <- replicateM numCapabilities $ newIORef []
   idle <- newIORef []
   let states = [ Sched { no=x, workpool=wp, idle, scheds=states }
                | (x,wp) <- zip [0..] workpools ]

#if __GLASGOW_HASKELL__ >= 701 /* 20110301 */
    --
    -- We create a thread on each CPU with forkOn.  The CPU on which
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
        forkOn cpu $
          if (cpu /= main_cpu)
             then reschedule state
             else do
                  rref <- newIORef Empty
                  sched _doSync state $ runCont (x >>= put_ (IVar rref)) (const Done)
                  readIORef rref >>= putMVar m

   r <- takeMVar m
   case r of
     Full a -> return a
     _ -> error "no result"


-- | Run a parallel, deterministic computation and return its result.
-- 
--   Note: you must NOT return an IVar in the output of the parallel
--   computation.  This is unfortunately not enforced, as it is with
--   `runST` or with newer libraries that export a Par monad, such as
--   `lvish`.
runPar :: Par a -> a
runPar = unsafePerformIO . runPar_internal True

-- | A version that avoids an internal `unsafePerformIO` for calling
--   contexts that are already in the `IO` monad.
--
--   Returning any value containing IVar is still disallowed, as it
--   can compromise type safety.
runParIO :: Par a -> IO a
runParIO = runPar_internal True

-- | An asynchronous version in which the main thread of control in a
-- Par computation can return while forked computations still run in
-- the background.
runParAsync :: Par a -> a
runParAsync = unsafePerformIO . runPar_internal False

-- -----------------------------------------------------------------------------

-- | Creates a new @IVar@
new :: Par (IVar a)
new  = Par $ New Empty

-- | Creates a new @IVar@ that contains a value
newFull :: NFData a => a -> Par (IVar a)
-- What are we doing here? We're manually raising the arity
-- of newFull from 2 to 3, which seems like it's probably what
-- we want most of the time. Notably, fmapping over the result
-- gives really awful-looking Core if we don't do this.
-- Regardless, I think we logically want to force the
-- value when it's installed in the IVar rather than
-- when we create the action to install it in the IVar.
newFull x = Par $ \c -> x `deepseq` New (Full x) c

-- | Creates a new @IVar@ that contains a value (head-strict only)
newFull_ :: a -> Par (IVar a)
newFull_ !x = Par $ New (Full x)

-- | Read the value in an @IVar@.  The 'get' operation can only return when the
-- value has been written by a prior or parallel @put@ to the same
-- @IVar@.
get :: IVar a -> Par a
get v = Par $ \c -> Get v c

-- | Like 'put', but only head-strict rather than fully-strict.
put_ :: IVar a -> a -> Par ()
put_ v !a = Par $ \c -> Put v a (c ())

-- | Put a value into an @IVar@.  Multiple 'put's to the same @IVar@
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
-- Manually raise the arity, which seems likely to be what
-- we want most of the time. We really want to force the
-- value when it's installed in the IVar, not when we
-- create the Par action to install it in the IVar.
put v a = Par $ \c -> a `deepseq` Put v a (c ())

-- | Allows other parallel computations to progress.  (should not be
-- necessary in most cases).
yield :: Par ()
yield = Par $ \c -> Yield (c ())
