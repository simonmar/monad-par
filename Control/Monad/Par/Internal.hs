{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP
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


import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent hiding (yield)
import GHC.Conc hiding (yield)
import Control.DeepSeq
import Control.Applicative
import Text.Printf
import System.Random

import qualified Data.Sequence as Seq 
import Data.Sequence hiding (length, null, replicateM, zip3, zip, splitAt, reverse)


-- TOGGLE #1: Select Deque implementation:
-- #define SEQDEQUES

-- TOGGLE #2: Select scheduling policy.  
-- Turn all three of the following on for cilk-style.
#define FORKPARENT  -- Fork parent cont rather than child.
#define STEALBACK   -- Steal from back rather than front.
#define RANDOMSTEAL -- Steal randomly rather than in order.

-- Turning this off is a bad idea.  It shows no benefit on parfib
-- presently, and it introduces a serious liability presently
-- (prematurely losing workers).
#define WAKEIDLE    -- Wake worker threads that gave up.

-- ---------------------------------------------------------------------------
-- Deque operations:
--   Could make a class of these if desired.  But during testing we
-- #ifdef to insure no dictionaries.

{-# INLINE addfront #-}
{-# INLINE addback  #-}
{-# INLINE takefront #-}
{-# INLINE takeback #-}

emptydeque :: Deque a 
addfront  :: a -> Deque a -> Deque a
addback   :: a -> Deque a -> Deque a

-- takefront :: Deque a -> Maybe (Deque a, a)
takefront :: Deque a -> (Deque a, Maybe a)
takeback  :: Deque a -> (Deque a, Maybe a)

-- [2011.03.21] Presently lists are out-performing Seqs:
#ifdef SEQDEQUES
newtype Deque a = DQ (Seq.Seq a)
emptydeque = DQ Seq.empty

addfront x (DQ s) = DQ (x <| s)
addback  x (DQ s) = DQ (s |> x)
takefront (DQ s) = 
  case Seq.viewl s of
    EmptyL  -> (emptydeque, Nothing)
    x :< s' -> (DQ s', Just x)

takeback (DQ s) = 
  case Seq.viewr s of
    EmptyR  -> (emptydeque, Nothing)
    s' :> x -> (DQ s', Just x)

#else
newtype Deque a = DQ [a]
emptydeque = DQ []

addfront x (DQ l)    = DQ (x:l)
addback x (DQ [])    = DQ [x]
addback x (DQ (h:t)) = DQ (h : rest)
 where DQ rest = addback x (DQ t)

takefront (DQ [])     = (emptydeque, Nothing)
takefront (DQ (x:xs)) = (DQ xs, Just x)
takeback  (DQ [])     = (emptydeque, Nothing)
takeback  (DQ ls)     = (DQ rest, Just final)
 where 
  -- This one space leaks:
  -- (final,rest) = loop ls
  -- loop [x] = (x,[])
  -- loop (h:tl) = let (last,rest) = loop tl in
  -- 	        (last, h:rest)

  -- Tail recursive version, this gets rid of the space leak:
  (final,rest) = loop ls []
  loop [x]    acc = (x, reverse acc)
  loop (h:tl) acc = loop tl (h:acc)
#endif


-- ---------------------------------------------------------------------------

data Trace = forall a . Get (IVar a) (a -> Trace)
           | forall a . Put (IVar a) a Trace
           | forall a . New (IVarContents a) (IVar a -> Trace)
           | Fork Trace Trace
           | Done
           | Yield Trace

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
#ifdef FORKPARENT
         pushWork queue parent
         loop child
#else
         pushWork queue child
         loop parent
#endif
    Done ->
         if _doSync
	 then reschedule queue
-- We could fork an extra thread here to keep numCapabilities workers
-- even when the main thread returns to the runPar caller...
#ifdef FORKPARENT
	 else error "FIXME: NO SOLUTION YET FOR ASYNC MODE WITH FORKPARENT"
#else
	 else do putStrLn " [par] Forking replacement thread..\n"; forkIO (reschedule queue); return ()
-- But even if we don't fork a replacement we are not orphaning any work in this
-- threads work-queue because it can be stolen by other threads.
--	 else return ()
#endif



    Yield parent -> do 
        -- Go to the end of the worklist:
        let Sched { workpool } = queue
        -- TODO: Perhaps consider Data.Seq here.
	-- This would also be a chance to steal and work from opposite ends of the queue.
        atomicModifyIORef workpool $ \ts -> (addback parent ts, ())
	reschedule queue

-- | Process the next item on the work queue or, failing that, go into
--   work-stealing mode.
reschedule :: Sched -> IO ()
reschedule queue@Sched{ workpool } = do
  e <- atomicModifyIORef workpool takefront 
  case e of
    Nothing -> steal queue
    Just t  -> sched True queue t


-- RRN: Note -- NOT doing random work stealing breaks the traditional
-- Cilk time/space bounds if one is running strictly nested (series
-- parallel) programs.

-- Returns a random index between 0 and numCapabilities-1
rand :: IORef StdGen -> IO Int
rand ioref = 
 do g <- readIORef ioref
    let (n,g') = next g
	i = n `mod` numCapabilities
    writeIORef ioref g'
    return i

dbg = False

-- | Attempt to steal work or, failing that, give up and go idle.
steal :: Sched -> IO ()
steal q@Sched{ idle, scheds, rng, no=my_no } = do
  when dbg$ printf "cpu %d stealing\n" my_no
  i <- getnext (-1)
  go maxtries i
 where
#ifdef WAKEIDLE
    maxtries = numCapabilities -- How many times should we attempt theft before going idle?
#else
    maxtries = 20 * numCapabilities -- More if they don't wake up after.
#endif

#ifdef RANDOMSTEAL
    getnext _ = rand rng
#else
    getnext i = return (i+1)
    go tries i | i >= numCapabilities = go tries 0
#endif

    go 0 _ = 
            do m <- newEmptyMVar
               r <- atomicModifyIORef idle $ \is -> (m:is, is)
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
			 i <- getnext (-1)
                         go maxtries i
    go tries i
#ifndef SELFSTEAL
      | i == my_no = do i' <- getnext i
			go (tries-1) i'
#endif
      | otherwise     = do
         let schd = scheds!!i
         when dbg$ printf "cpu %d trying steal from %d\n" my_no (no schd)
#ifdef STEALBACK
         r <- atomicModifyIORef (workpool schd) takeback
#else
         r <- atomicModifyIORef (workpool schd) takefront
#endif
         case r of
           Just t  -> do
              when dbg$ printf "cpu %d got work from cpu %d\n" my_no (no schd)
              sched True q t
           Nothing -> do i' <- getnext i
			 go (tries-1) i'

-- | If any worker is idle, wake one up and give it work to do.
pushWork :: Sched -> Trace -> IO ()
pushWork Sched { workpool, idle } t = do
  atomicModifyIORef workpool $ \ts -> (addfront t ts, ())
#ifdef WAKEIDLE
  idles <- readIORef idle
  when (not (null idles)) $ do
    r <- atomicModifyIORef idle (\is -> case is of
                                          [] -> ([], return ())
                                          (i:is) -> (is, putMVar i False))
    r -- wake one up
#endif

data Sched = Sched
    { no       :: {-# UNPACK #-} !Int,
      workpool :: IORef (Deque Trace),
      idle     :: IORef [MVar Bool],
      rng      :: IORef StdGen, -- Random number gen for this worker thread
      -- Global list of all per-thread workers:
      scheds :: [Sched]
    }
--  deriving Show

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


data IVarContents a = Full a | Empty | Blocked [a -> Trace]


{-# INLINE runPar_internal #-}
runPar_internal :: Bool -> Par a -> a
runPar_internal _doSync x = unsafePerformIO $ do
   workpools <- replicateM numCapabilities $ newIORef emptydeque
   idle    <- newIORef []
   newrngs <- replicateM numCapabilities (newStdGen >>= newIORef)
   let 
       states = [ Sched { no=x, workpool=wp, idle, rng=r, scheds=states }
                | (x,wp,r) <- zip3 [0..] workpools newrngs ]

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
             then reschedule state
             else do
                  rref <- newIORef Empty
                  sched _doSync state $ runCont (x >>= put_ (IVar rref)) (const Done)
                  readIORef rref >>= putMVar m

   r <- takeMVar m
   case r of
     Full a -> return a
     _ -> error "no result"


runPar :: Par a -> a
runPar = runPar_internal True

-- | An asynchronous version in which the main thread of control in a
-- Par computation can return while forked computations still run in
-- the background.  
runParAsync :: Par a -> a
runParAsync = runPar_internal False

-- | An alternative version in which the consumer of the result has
-- | the option to "help" run the Par computation if results it is
-- | interested in are not ready yet.
runParAsyncHelper :: Par a -> (a, IO ())
runParAsyncHelper = undefined -- TODO: Finish Me.

-- -----------------------------------------------------------------------------

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

-- | Allows other parallel computations to progress.
yield :: Par ()
yield = Par $ \c -> Yield (c ())
