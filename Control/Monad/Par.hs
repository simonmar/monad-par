{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP
	     #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fwarn-unused-imports #-}

-- | This module provides a deterministic parallelism monad, @Par@.
-- @Par@ is for speeding up pure computations; it cannot be used for
-- IO (for that, see @Control.Concurrent@).  The result of a given
-- @Par@ computation is always the same - ie. it is deterministic, but
-- the computation may be performed more quickly if there are
-- processors available to share the work.
--
-- For example, the following program fragment computes the values of
-- @(f x)@ and @(g x)@ in parallel, and returns a pair of their results:
--
-- >  runPar $ do
-- >      a <- pval (f x)
-- >      b <- pval (g x)
-- >      return (a,b)
--
-- @Par@ can be used for specifying pure parallel computations in
-- which the order of the computation is not known beforehand;
-- that is, the programmer specifies how information flows from one
-- part of the computation to another, but not the order in which
-- computations will be evaluated at runtime.  Information flow is
-- described using "variables" called @IVar@s, which support 'put' and
-- 'get' operations.  For example, the following defines a small
-- network with 4 data values in a diamond-shaped data flow:
--
-- >   runPar $ do
-- >       [a,b,c,d] <- sequence [new,new,new,new]
-- >       fork $ do x <- get a; put b (x+1)
-- >       fork $ do x <- get a; put c (x+2)
-- >       fork $ do x <- get b; y <- get c; put d (x+y)
-- >       fork $ do put a (3 :: Int)
-- >       get d
--
-- The result of this computation is always 9.  The 'get' operation
-- waits until its input is available; multiple 'put's to the same
-- @IVar@ are not allowed, and result in a runtime error.  Values
-- stored in @IVar@s are usually fully evaluated (although there are
-- ways provided to pass lazy values if necessary).
--
-- Unlike @Control.Parallel@, in @Control.Monad.Par@ parallelism is
-- not combined with laziness, so sharing and granulairty are
-- completely under the control of the programmer.  New units of
-- parallel work are only created by @fork@, @par@, and a few other
-- combinators.
--
-- The implementation is based on a work-stealing scheduler that
-- divides the work as evenly as possible betwen the available
-- processors at runtime.
--

module Control.Monad.Par (
    Par, IVar, pollIVar,
    runPar, runParAsync,
    fork,
    yield,
    new, newFull, newFull_,
    get,
    put, put_,
    both,
    pval,
    spawn, spawn_,
    parMap, parMapM, parMapReduceRange, parMapReduceRangeThresh,
    InclusiveRange(..),
    parFor

-- TEMP:
, _async_test1, _async_test2

  ) where

import Data.Traversable
import Control.Monad as M hiding (mapM, sequence, join) 
import Prelude hiding (mapM, sequence, head,tail)
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent hiding (yield)
import GHC.Conc hiding (yield)
import Control.DeepSeq
import Control.Applicative
-- import Text.Printf

-- For testing only:
import Test.HUnit 
import Control.Exception

import qualified Data.Sequence as Seq

-- TOGGLE #1: Select Deque implementation:
-- #define SEQDEQUES

-- TOGGLE #2: Select scheduling policy.  Turn both on for cilk-style.
-- #define PARENTSTEAL
-- #define RANDOMSTEAL

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

#ifdef SEQDEQUES
#else
newtype Deque a = DQ [a]

emptydeque = DQ []

addfront x (DQ l) = DQ (x:l)

addback x (DQ [])    = DQ [x]
addback x (DQ (h:t)) = DQ (h : rest)
 where DQ rest = addback x (DQ t)

takefront (DQ [])     = (emptydeque, Nothing)
takefront (DQ (x:xs)) = (DQ xs, Just x)

takeback (DQ []) = (emptydeque, Nothing)
takeback (DQ ls) = (DQ rest, Just final)
 where 
  (final,rest) = loop ls
  loop [x] = (x,[])
  loop (h:tl) = let (last,rest) = loop tl in
	        (last, h:rest)
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
         pushWork queue child
         loop parent
    Done ->
         if _doSync
	 then reschedule queue
-- We could fork an extra thread here to keep numCapabilities workers
-- even when the main thread returns to the runPar caller...
	 else do putStrLn " [par] Forking replacement thread..\n"; forkIO (reschedule queue); return ()
-- But even if we don't we are not orphaning any work in this
-- threads work-queue because it can be stolen by other threads.
--	 else return ()

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
-- parallel) programs (e.g. spawn with no get).

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
         r <- atomicModifyIORef (workpool x) takefront
         case r of
           Just t  -> do
              -- printf "cpu %d got work from cpu %d\n" my_no (no x)
              sched True q t
           Nothing -> go xs

-- | If any worker is idle, wake one up and give it work to do.
pushWork :: Sched -> Trace -> IO ()
pushWork Sched { workpool, idle } t = do
  atomicModifyIORef workpool $ \ts -> (addfront t ts, ())
  idles <- readIORef idle
  when (not (null idles)) $ do
    r <- atomicModifyIORef idle (\is -> case is of
                                          [] -> ([], return ())
                                          (i:is) -> (is, putMVar i False))
    r -- wake one up

data Sched = Sched
    { no       :: {-# UNPACK #-} !Int,
      workpool :: IORef (Deque Trace),
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
   idle <- newIORef []
   let states = [ Sched { no=x, workpool=wp, idle, scheds=states }
                | (x,wp) <- zip [0..] workpools ]

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

-- | forks a computation to happen in parallel.  The forked
-- computation may exchange values with other computations using
-- @IVar@s.
fork :: Par () -> Par ()
fork p = Par $ \c -> Fork (runCont p (\_ -> Done)) (c ())

-- > both a b >> c  ==   both (a >> c) (b >> c)
-- is this useful for anything?
both :: Par a -> Par a -> Par a
both a b = Par $ \c -> Fork (runCont a c) (runCont b c)


-- | Allows other parallel computations to progress.
yield :: Par ()
yield = Par $ \c -> Yield (c ())

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

-- -----------------------------------------------------------------------------
-- Derived functions

-- | Like 'spawn', but the result is only head-strict, not fully-strict.
spawn_ :: Par a -> Par (IVar a)
spawn_ p = do
  r <- new
  fork (p >>= put_ r)
  return r

-- | Like 'fork', but returns a @IVar@ that can be used to query the
-- result of the forked computataion.
spawn :: NFData a => Par a -> Par (IVar a)
spawn p = do
  r <- new
  fork (p >>= put r)
  return r

-- | equivalent to @spawn . return@
pval :: NFData a => a -> Par (IVar a)
pval a = spawn (return a)

-- -----------------------------------------------------------------------------
-- Parallel maps over Traversable data structures

parMap :: (Traversable t, NFData b) => (a -> b) -> t a -> Par (t b)
parMap f xs = mapM (pval . f) xs >>= mapM get

parMapM :: (Traversable t, NFData b) => (a -> Par b) -> t a -> Par (t b)
parMapM f xs = mapM (spawn . f) xs >>= mapM get

{-# SPECIALISE parMap  :: (NFData b) => (a -> b)     -> [a] -> Par [b] #-}
{-# SPECIALISE parMapM :: (NFData b) => (a -> Par b) -> [a] -> Par [b] #-}


-- TODO: Perhaps should introduce a class for the "splittable range" concept.
data InclusiveRange = InclusiveRange Int Int 

-- | parMapReduceRange is similar to the "parallel for" construct
--   found in many parallel programming models in that it takes a
--   numeric start/end range as its input domain.
parMapReduceRangeThresh :: NFData a => 
                           Int -> InclusiveRange -> (Int -> Par a) -> (a -> a -> Par a) -> a -> Par a
parMapReduceRangeThresh threshold (InclusiveRange min max) fn binop init = loop min max 
 where 
  loop min max 
    | max - min <= threshold = 
	let mapred a b = do x <- fn b; 
			    result <- a `binop` x
			    return result 
	in foldM mapred init [min..max]

    | otherwise  = do
	let mid = min + ((max - min) `quot` 2)
	rght <- spawn $ loop (mid+1) max 
	l  <- loop  min    mid 
	r  <- get rght
	l `binop` r

-- How many tasks per process should we aim for.  Higher numbers
-- improve load balance but put more pressure on the scheduler.
auto_partition_factor = 4

-- | "Auto-partitioning" versiobn that chooses the threshold based on
--    the size of the range and the number of processors..
parMapReduceRange :: NFData a => InclusiveRange -> (Int -> Par a) -> (a -> a -> Par a) -> a -> Par a
parMapReduceRange (InclusiveRange start end) fn binop init = 
   loop (length segs) segs
 where 
  segs = splitInclusiveRange (auto_partition_factor * numCapabilities) (start,end)
  loop 1 [(st,en)] = 
     let mapred a b = do x <- fn b; 
			 result <- a `binop` x
			 return result 
     in foldM mapred init [st..en]
  loop n segs = 
     let half = n `quot` 2
	 (left,right) = splitAt half segs in
     do l  <- spawn$ loop half left
        r  <- loop (n-half) right
	l' <- get l
	l' `binop` r


-- TODO: A version that works for any splittable input domain.  In this case
-- the "threshold" is a predicate on inputs.
-- parMapReduceRangeGeneric :: (inp -> Bool) -> (inp -> Maybe (inp,inp)) -> inp -> 


-- Experimental:

-- Parallel for-loop over an inclusive range.  Executes the body for
-- put-effects only.  This could be implemented with parMapReduceRange
-- but this version has no reduce function...
parFor :: InclusiveRange -> (Int -> Par ()) -> Par ()
parFor (InclusiveRange start end) body = 
 do 
    let run (x,y) = for_ x (y+1) body
        range_segments = splitInclusiveRange (4*numCapabilities) (start,end)

    vars <- M.forM range_segments (\ pr -> spawn_ (run pr))
    M.mapM_ get vars
    return ()

splitInclusiveRange :: Int -> (Int, Int) -> [(Int, Int)]
splitInclusiveRange pieces (start,end) = 
  map largepiece [0..remain-1] ++ 
  map smallpiece [remain..pieces-1]
 where 	
   len = end - start + 1 -- inclusive [start,end]
   (portion, remain) = len `quotRem` pieces
   largepiece i = 
       let offset = start + (i * (portion + 1))
       in (offset, offset + portion)
   smallpiece i = 
       let offset = start + (i * portion) + remain
       in (offset, offset + portion - 1)

-- My own forM for numeric ranges (not requiring deforestation optimizations).
-- Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for_ start end fn | start > end = error "for_: start is greater than end"
for_ start end fn = loop start 
 where 
  loop !i | i == end  = return ()
	  | otherwise = do fn i; loop (i+1) 



-- -----------------------------------------------------------------------------
-- Testing

_test :: IO ()
_test = do
  print ((runPar $ return 3) :: Int)
  print (runPar $ do r <- new; put r (3 :: Int); get r)
  print (runPar $ do r <- new; fork (put r (3::Int)); get r)
  print ((runPar $ do r <- new; get r)  :: Int)

_test2 :: Int
_test2 =  runPar $ do
      [a,b,c,d] <- sequence [new,new,new,new]
      fork $ do x <- get a; put b (x+1)
      fork $ do x <- get a; put c (x+2)
      fork $ do x <- get b; y <- get c; put d (x+y)
      fork $ do put a 3
      get d

_test3 :: Int
_test3 = runPar $ do
   a <- new
   put a (3::Int)
   both (return 1) (return 2)

-- is there a standard lib thing for this?

_test_pmrr1 :: Int
_test_pmrr1 = runPar$ parMapReduceRangeThresh 1 (InclusiveRange 1 100) 
	                                      (return) (return `bincomp` (+)) 0
 where bincomp unary bin a b = unary (bin a b)

_unsafeio :: IO a -> Par a
_unsafeio io = let x = unsafePerformIO io in
	        x `seq` return x

_print :: String -> Par ()
_print = _unsafeio . putStrLn

_waste_time :: Int -> Double
_waste_time n = loop n 1.00111
  where 
    loop 0  !x             = x
    loop !n !x | x > 100.0 = loop (n-1) (x / 2)
    loop !n !x             = loop (n-1) (x + x * 0.5011)


_async_test1 = do  -- A D B <pause> C E 
  putStrLn "A"
  evaluate$ runPar $
    do 
       fork $ do _print "B"
		 _print$ "C "++ show (_waste_time 300000000)
       _print "D"
  putStrLn$ "E"

_async_test2 = do  -- A D E  or A D B E  but no C
  putStrLn "A"
  evaluate$ runParAsync $
    do 
       fork $ do _print "B"
		 _print$ "C "++ show (_waste_time 300000000)
       _print "D"
  putStrLn$ "E"


-- TODO: add the async_tests above to the test list.
_par_tests :: Test
_par_tests = TestList []
