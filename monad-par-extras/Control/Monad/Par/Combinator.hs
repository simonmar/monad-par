{-# LANGUAGE BangPatterns #-}
{-| 
    A collection of useful parallel combinators based on top of a 'Par' monad.

    In particular, this module provides higher order functions for
     traversing data structures in parallel.  

-}

module Control.Monad.Par.Combinator
  (
    parMap, parMapM,
    parMapReduceRangeThresh, parMapReduceRange,
    InclusiveRange(..),
    parFor
  )
where 

import Control.DeepSeq
import Data.Traversable
import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import GHC.Conc (numCapabilities)

import Control.Monad.Par.Class

-- -----------------------------------------------------------------------------
-- Parallel maps over Traversable data structures

-- | Applies the given function to each element of a data structure
-- in parallel (fully evaluating the results), and returns a new data
-- structure containing the results.
--
-- > parMap f xs = mapM (spawnP . f) xs >>= mapM get
--
-- @parMap@ is commonly used for lists, where it has this specialised type:
--
-- > parMap :: NFData b => (a -> b) -> [a] -> Par [b]
--
parMap :: (Traversable t, NFData b, ParFuture iv p) => (a -> b) -> t a -> p (t b)
parMap f xs = mapM (spawnP . f) xs >>= mapM get

-- | Like 'parMap', but the function is a @Par@ monad operation.
--
-- > parMapM f xs = mapM (spawn . f) xs >>= mapM get
--
parMapM :: (Traversable t, NFData b, ParFuture iv p) => (a -> p b) -> t a -> p (t b)
parMapM f xs = mapM (spawn . f) xs >>= mapM get

-- TODO: parBuffer



-- --------------------------------------------------------------------------------

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
   :: (NFData a, ParFuture iv p)
      => Int                            -- ^ threshold
      -> InclusiveRange                 -- ^ range over which to calculate
      -> (Int -> p a)                 -- ^ compute one result
      -> (a -> a -> p a)              -- ^ combine two results (associative)
      -> a                              -- ^ initial result
      -> p a

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

-- How many tasks per process should we aim for?  Higher numbers
-- improve load balance but put more pressure on the scheduler.
auto_partition_factor :: Int
auto_partition_factor = 4

-- | \"Auto-partitioning\" version of 'parMapReduceRangeThresh' that chooses the threshold based on
--    the size of the range and the number of processors..
parMapReduceRange :: (NFData a, ParFuture iv p) => 
		     InclusiveRange -> (Int -> p a) -> (a -> a -> p a) -> a -> p a
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

parFor :: (ParFuture iv p) => InclusiveRange -> (Int -> p ()) -> p ()
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
