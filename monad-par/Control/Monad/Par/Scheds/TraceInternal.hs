{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns, LambdaCase,
             ExistentialQuantification, CPP, MagicHash, UnboxedTuples #-}
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
   pollIVar, yield,
 ) where


import Control.Monad as M hiding (mapM, sequence)
import Prelude hiding (mapM, sequence, head,tail)
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent hiding (yield)
import GHC.Conc (numCapabilities)
import Control.DeepSeq
import GHC.IO (IO (..))
import GHC.IORef (IORef (..))
import GHC.STRef (STRef (..))
import GHC.Exts (casMutVar#, lazy, MutVar#, RealWorld)
--import Control.Exception (evaluate)
-- import Text.Printf

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

#if __GLASGOW_HASKELL__ <= 700
import GHC.Conc (forkOnIO)
forkOn = forkOnIO
#endif


-- ---------------------------------------------------------------------------

data Trace = forall a . Get !(IVar a) (a -> Trace)
           | forall a . Put !(IVar a) a Trace
             -- Why does the continuation take a MutVar# instead of an IVar?
             -- In the common case that we don't actually *need* the IVar wrapper,
             -- we'd rather not allocate it.
           | forall a . New !(IVarContents a) (MutVar# RealWorld (IVarContents a) -> Trace)
           | Fork Trace Trace
           | Done
           | Yield Trace
           | forall a . LiftIO (IO a) (a -> Trace)

-- |A thin wrapper around 'casMutVar#'.
casIORef_ :: IORef a -> a -> a -> IO Bool
casIORef_ (IORef (STRef ref)) old new =
  IO $ \s -> case casMutVar# ref old new s of
               (# s', 0#, _latest #) -> (# s', True #)
               (# s', _, _latest #) -> (# s', False #)

-- This is similar to atomicModifyIORef, but it applies the
-- function eagerly. This should only be used when the function
-- is very cheap: it must take very little time to avoid CAS
-- failures, and it should not allocate *any* memory, since it
-- will be run again on CAS failure.
atomicModifyIORefEager :: IORef a -> (a -> (# Maybe a, b #)) -> IO b
atomicModifyIORefEager ref f = go ref
  where
    go ref = do
      old <- readIORef ref
      -- The lazy here is a bit paranoid; we want to be sure that GHC
      -- doesn't decide to substitute an unfolding for old in the call
      -- to casIORef. I don't know if that can happen anyway, but
      -- being extra-careful shouldn't hurt.
      case f (lazy old) of
        (# Nothing, res #) -> pure res
        (# Just new, res #) -> do
          done <- casIORef_ ref old new
          if done
            then pure res
            else go ref
{-# INLINE atomicModifyIORefEager #-}


-- | The main scheduler loop.
sched :: Bool -> Sched -> Trace -> IO ()
sched _doSync !queue t = loop t
 where
  loop t = case t of
    New a f -> do
      IORef (STRef r) <- newIORef a
      loop (f r)
    Get (IVar v) c -> do
      -- Optimistic check for Full avoids the expense of
      -- atomicModifyIORef
      e0 <- readIORef v
      case e0 of
         Full a -> loop (c a)
         _ -> do
           r <- atomicModifyIORef v $ \e -> case e of
                        Full a     -> (Full a,         Just a)
                        cs         -> (Blocked c cs, Nothing)
           maybe (reschedule queue) (loop . c) r
    Put (IVar v) a t  -> do
      let fulla = Full a
      cs <- atomicModifyIORefEager v $ \case
        Full _ -> error "multiple put"
        cs -> (# Just fulla, cs #)
      icmapM_ (pushWork queue . ($a)) cs
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
        -- TODO: Perhaps consider some sort of deque here
        -- This would also be a chance to steal and work from opposite ends of the queue.
        () <- atomicModifyIORef workpool $ \ts -> (ts++[parent], ())
        reschedule queue
    LiftIO io c -> do
        r <- io
        loop (c r)

-- | Process the next item on the work queue or, failing that, go into
--   work-stealing mode.
reschedule :: Sched -> IO ()
reschedule queue@Sched{ workpool } = do
       -- We don't use atomicModifyIORefEager for workpools, because
       -- yields can cause those to contain thunks, and forcing thunks
       -- can easily lead to CAS failures.
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
               r <- atomicModifyIORef idle $ \is -> (m `ICons` is, is)
               if ilength r == numCapabilities - 1
                  then do
                     -- printf "cpu %d initiating shutdown\n" my_no
                     imapM_ (\m -> putMVar m True) r
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
         -- Cheaply skip over empty workpools.
         r0 <- readIORef (workpool x)
         case r0 of
           [] -> go xs
           _ -> do
             r <- atomicModifyIORef (workpool x) $ \ ts ->
                     case ts of
                        []     -> ([], Nothing)
                        (t:ts') -> (ts', Just t)
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
  when (not (inull idles)) $ do
    r <- atomicModifyIORefEager idle (\is -> case lazy is of
                                          INil -> (# Nothing, INil #)
                                          (_ `ICons` is') -> (# Just is', is #))
    case r of
      INil -> pure ()
      i `ICons` _ -> putMVar i False -- wake one up

data Sched = Sched
    { no       :: {-# UNPACK #-} !Int,
      workpool :: !(IORef [Trace]),
      idle     :: !(IORef IdleList),
      scheds   :: [Sched] -- Global list of all per-thread workers.
    }
--  deriving Show

data IdleList = ICons !(MVar Bool) IdleList | INil
inull :: IdleList -> Bool
inull INil = True
inull (ICons _ _) = False

imapM_ :: Applicative f => (MVar Bool -> f a) -> IdleList -> f ()
imapM_ f = go
  where
    go INil = pure ()
    go (ICons x xs) = f x *> go xs

ilength :: IdleList -> Int
ilength = go 0 where
  go !acc INil = acc
  go acc (ICons _ xs) = go (acc + 1) xs

icmapM_ :: Applicative f => ((a -> Trace) -> f b) -> IVarContents a -> f ()
icmapM_ f = go
  where
    go (Blocked c cs) = f c *> go cs
    go _ = pure ()

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

newtype IVar a = IVar (IORef (IVarContents a))
-- data IVar a = IVar (IORef (IVarContents a))

-- | Equality for IVars is physical equality, as with other reference types.
instance Eq (IVar a) where
  (IVar r1) == (IVar r2) = r1 == r2

-- We just force the IORef; the contents can't be forced.
instance NFData (IVar a) where
  rnf !_ = ()


-- From outside the Par computation we can peek.  But this is nondeterministic.
pollIVar :: IVar a -> IO (Maybe a)
pollIVar (IVar ref) =
  do contents <- readIORef ref
     case contents of
       Full x    -> return (Just x)
       _         -> return Nothing


-- Invariant: Full should only appear in an outermost position.
-- Something like  Blocked f (Full a)  is prohibited.
-- We used to use  Blocked [a -> Trace], but that requires
-- an extra wrapper allocation to add an element.
data IVarContents a = Full a | Empty | Blocked (a -> Trace) !(IVarContents a)

-- A strict-spined list of unboxed IORefs.
data IORefList a = RLCons !(IORef a) !(IORefList a) | RLNil

buildRL :: Int -> IO (IORefList [a])
buildRL = go RLNil
  where
    go acc 0 = pure acc
    go acc n = do
      ref <- newIORef []
      go (RLCons ref acc) (n - 1)

buildStates :: Int
            -> IORefList [Trace] -> IORef IdleList -> [Sched] -> [Sched]
buildStates !_k RLNil !_idl _states = []
buildStates k (RLCons wp wps) !idl states =
  Sched { no = k, workpool=wp, idle = idl, scheds=states } :
   buildStates (k + 1) wps idl states

{-# INLINE runPar_internal #-}
runPar_internal :: Bool -> Par a -> IO a
runPar_internal _doSync x = do
   workpools <- buildRL numCapabilities
   idle <- newIORef INil
   let states = buildStates 0 workpools idle states

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
new = Par $ \c -> New Empty $ \mv -> c (IVar (IORef (STRef mv)))

-- | Creates a new @IVar@ that contains a value
newFull :: NFData a => a -> Par (IVar a)
newFull x = deepseq x (newFull_ x)

-- | Creates a new @IVar@ that contains a value (head-strict only)
newFull_ :: a -> Par (IVar a)
newFull_ !x = Par $ \c -> New (Full x) $ \mv -> c (IVar (IORef (STRef mv)))

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
put v a = deepseq a (Par $ \c -> Put v a (c ()))

-- | Allows other parallel computations to progress.  (should not be
-- necessary in most cases).
yield :: Par ()
yield = Par $ \c -> Yield (c ())
