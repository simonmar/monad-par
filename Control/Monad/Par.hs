{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification
	     #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fwarn-unused-imports #-}

-- | This module provides a monad @Par@, for speeding up pure
-- computations using parallel processors.  It cannot be used for
-- speeding up computations that use IO (for that, see
-- @Control.Concurrent@).  The result of a given @Par@ computation is
-- always the same - ie. it is deterministic, but the computation may
-- be performed more quickly if there are processors available to
-- share the work.
--
-- For example, the following program fragment computes the values of
-- @(f x)@ and @(g x)@ in parallel, and returns a pair of their results:
--
-- >  runPar $ do
-- >      fx <- pval (f x)  -- start evaluating (f x)
-- >      gx <- pval (g x)  -- start evaluating (g x)
-- >      a <- get fx       -- wait for fx
-- >      b <- get gx       -- wait for gx
-- >      return (a,b)      -- return results
--
-- @Par@ can be used for specifying pure parallel computations in
-- which the order of the computation is not known beforehand.
-- The programmer specifies how information flows from one
-- part of the computation to another, but not the order in which
-- computations will be evaluated at runtime.  Information flow is
-- described using "variables" called @IVar@s, which support 'put' and
-- 'get' operations.  For example, suppose you have a problem that
-- can be expressed as a network with four nodes, where @b@ and @c@
-- require the value of @a@, and @d@ requires the value of @b@ and @c@:
--
-- >                       a
-- >                      / \  
-- >                     b   c
-- >                      \ /
-- >                       d
--
-- Then you could express this in the @Par@ monad like this:
--
-- >   runPar $ do
-- >       [a,b,c,d] <- sequence [new,new,new,new]
-- >       fork $ do x <- get a; put b (x+1)
-- >       fork $ do x <- get a; put c (x+2)
-- >       fork $ do x <- get b; y <- get c; put d (x+y)
-- >       fork $ do put a (3 :: Int)
-- >       get d
--
-- The result of the above computation is always 9.  The 'get' operation
-- waits until its input is available; multiple 'put's to the same
-- @IVar@ are not allowed, and result in a runtime error.  Values
-- stored in @IVar@s are usually fully evaluated (although there are
-- ways provided to pass lazy values if necessary).
--
-- In the above example, @b@ and @c@ will be evaluated in parallel.
-- In practice the work involved at each node is too small here to see
-- the benefits of parallelism though: typically each node should
-- involve much more work.  The granularity is completely under your
-- control - too small and the overhead of the @Par@ monad will
-- outweigh any parallelism benefits, whereas if the nodes are too
-- large then there might not be enough parallelism to use all the
-- available processors.
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
    -- * The @Par@ monad
    Par,
    runPar,
    fork,

    -- * Communication: @IVar@s
    IVar,
    new, newFull, newFull_,
    get,
    put, put_,

    -- * Operations
    pval,
    spawn, spawn_,
    parMap, parMapM,
    parMapReduceRangeThresh, parMapReduceRange,
    InclusiveRange(..),
    parFor,

  ) where

import Control.Monad.Par.Internal
import Control.DeepSeq
import Data.Traversable
import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)

import GHC.Conc (numCapabilities)

-- -----------------------------------------------------------------------------

-- | forks a computation to happen in parallel.  The forked
-- computation may exchange values with other computations using
-- @IVar@s.
fork :: Par () -> Par ()
fork p = Par $ \c -> Fork (runCont p (\_ -> Done)) (c ())

-- > both a b >> c  ==   both (a >> c) (b >> c)
-- is this useful for anything?
-- both :: Par a -> Par a -> Par a
-- both a b = Par $ \c -> Fork (runCont a c) (runCont b c)

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
--
-- >  spawn p = do
-- >    r <- new
-- >    fork (p >>= put r)
-- >    return r
--
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

-- | Applies the given function to each element of a data structure
-- in parallel (fully evaluating the results), and returns a new data
-- structure containing the results.
--
-- > parMap f xs = mapM (pval . f) xs >>= mapM get
--
-- @parMap@ is commonly used for lists, where it has this specialised type:
--
-- > parMap :: NFData b => (a -> b) -> [a] -> Par [b]
--
parMap :: (Traversable t, NFData b) => (a -> b) -> t a -> Par (t b)
parMap f xs = mapM (pval . f) xs >>= mapM get

-- | Like 'parMap', but the function is a @Par@ monad operation.
--
-- > parMapM f xs = mapM (spawn . f) xs >>= mapM get
--
parMapM :: (Traversable t, NFData b) => (a -> Par b) -> t a -> Par (t b)
parMapM f xs = mapM (spawn . f) xs >>= mapM get

{-# SPECIALISE parMap  :: (NFData b) => (a -> b)     -> [a] -> Par [b] #-}
{-# SPECIALISE parMapM :: (NFData b) => (a -> Par b) -> [a] -> Par [b] #-}


-- TODO: Perhaps should introduce a class for the "splittable range" concept.
data InclusiveRange = InclusiveRange Int Int

-- | Computes a binary map\/reduce over a finite range.  The range is
-- recursively split into two, the result for each half is computed in
-- parallel, and then the two results are combined.  When the range
-- reaches the threshold size, the remaining elements of the range are
-- computed sequentially.
--
-- For example, the following is a parallel implementation of
--
-- >  foldl (+) 0 (map (^2) [1..10^6])
--
-- > parMapReduceRangeThresh 100 (InclusiveRange 1 (10^6))
-- >        (\x -> return (x^2))
-- >        (\x y -> return (x+y))
-- >        0
--
parMapReduceRangeThresh
   :: NFData a
      => Int                            -- ^ threshold
      -> InclusiveRange                 -- ^ range over which to calculate
      -> (Int -> Par a)                 -- ^ compute one result
      -> (a -> a -> Par a)              -- ^ combine two results (associative)
      -> a                              -- ^ initial result
      -> Par a

parMapReduceRangeThresh threshold (InclusiveRange min max) fn binop init
 = loop min max
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
auto_partition_factor :: Int
auto_partition_factor = 4

-- | \"Auto-partitioning\" version of 'parMapReduceRangeThresh' that chooses the threshold based on
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

-- | Parallel for-loop over an inclusive range.  Semantically equivalent
-- to
-- 
-- > parFor (InclusiveRange n m) f = forM_ [n..m] f
--
-- except that the implementation will split the work into an
-- unspecified number of subtasks in an attempt to gain parallelism.
-- The exact number of subtasks is chosen at runtime, and is probably
-- a small multiple of the available number of processors.
--
-- Strictly speaking the semantics of 'parFor' depends on the
-- number of processors, and its behaviour is therefore not
-- deterministic.  However, a good rule of thumb is to not have any
-- interdependencies between the elements; if this rule is followed
-- then @parFor@ has deterministic semantics.  One easy way to follow
-- this rule is to only use 'put' or 'put_' in @f@, never 'get'.

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
for_ start end _fn | start > end = error "for_: start is greater than end"
for_ start end fn = loop start
 where
  loop !i | i == end  = return ()
	  | otherwise = do fn i; loop (i+1)



