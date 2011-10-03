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

import qualified Data.Array as A
import Data.Traversable
import Control.Monad as M hiding (mapM, sequence, join) 
import Prelude hiding (mapM, sequence, head,tail)
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent hiding (yield)
import GHC.Conc hiding (yield)
import Control.DeepSeq
import Control.Applicative
import Debug.Trace
import System.Exit

-- For testing only:
import Test.HUnit 
import Control.Exception
import Control.Monad.Par.Internal

-- > both a b >> c  ==   both (a >> c) (b >> c)
-- is this useful for anything?
both :: Par a -> Par a -> Par a
both a b = Par $ \c -> Fork (runCont a c) (runCont b c)

-- | forks a computation to happen in parallel.  The forked
-- computation may exchange values with other computations using
-- @IVar@s.
fork :: Par () -> Par ()
fork p = Par $ \c -> Fork (runCont p (\_ -> Done)) (c ())

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
