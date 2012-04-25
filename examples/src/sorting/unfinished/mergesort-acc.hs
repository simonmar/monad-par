{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- UNFINISHED:

-- This file represents rebecca's attempt to port the IPDPS'09
-- mergesort algorithm to Accelerate.

--------------------------------------------------------------------------------

import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Interpreter (run)
import Prelude           hiding (zip, map, scanl, scanr, zipWith, fst, scanl1)
import Data.Array.IArray hiding ((!))
import Data.Array.Unboxed       (UArray, IArray)
import Data.Array.IO            (MArray, IOUArray)
import qualified Data.Array.MArray as M

--import Control.Monad.Par.Meta.SharedMemoryAccelerate

import Control.Exception  (evaluate)
import System.Environment (getArgs)
import System.Random ()
import System.Random.MWC  (uniformR, withSystemRandom, Variate, GenIO)

--------------------------------------------------------------------------------

{-
main :: IO ()
main = withSystemRandom $ \gen -> do
    args <- getArgs
    let n = case args of
              []  -> 10000 :: Int
              [n] -> read n
              _   -> error "usage: mergesort_acc.exe [size]"

    vec <- randomUArrayR (minBound,maxBound) gen n
    vec' <- convertUArray vec
    print $ run $ sortAcc vec'
-}
    {-
    print $ run_par vec' ()
    where
        run_par xs () = sortAcc xs
        run_par xs () = runPar $ do
            ans <- spawnAcc $ run_acc (xs :: Vector Int) ()
            get ans
        run_acc xs () = sortAcc xs
        -}


-- Generate a random, uniformly distributed vector of specified size over the
-- range. From Hackage documentation on uniformR: For integral types the range is 
-- inclusive. For floating point numbers, the range (a,b] is used, if one ignores 
-- rounding errors.
randomUArrayR :: (Int, Int) -> GenIO -> Int -> IO (UArray Int Int)
randomUArrayR lim gen n = do
    mu <- M.newArray_ (0,n-1) :: MArray IOUArray e IO => IO (IOUArray Int e)
    let go !i | i < n     = uniformR lim gen >>= M.writeArray mu i >> go (i+1)
              | otherwise = M.unsafeFreeze mu
    go 0

-- Convert an Unboxed Data.Array to an Accelerate Array
convertUArray :: UArray Int Int -> IO (Acc.Vector Int)
convertUArray v = 
    let arr = Acc.fromIArray v in
        evaluate (arr `Acc.indexArray` (Z:.0)) >> return arr

{- 
 - Broad overview:
 -   1. Split data into k blocks
 -   2. Sort each block in parallel with bitonic sort
 -   3. Perform log k steps of pairwise merging
 -      a. To merge two blocks, sample several every 256th element from each 
 -         block
 -      b. Compute the ranks of each sample in each array being merged:
 -          i.  the rank in its own array is just its index
 -          ii. the rank in the other array is found by binary search, which 
 -              can be bounded using knowledge about samples chosen from that 
 -              array
 -      c. These ranks define the boundaries of the sub-blocks of the final 
 -         result
 -      d. Using this, compute the rank of each element, which will be the sum 
 -         of its ranks in the two sub-blocks being merged
 -          i.  the rank in its own array is its index
 -          ii. the rank in the other array is found by binary search (using 
 -              map)
 -      e. Scatter: each thread (from the previous step) writes out its 
 -         element to the correct position. 
 -
 - NOTE: The bin-searching can be expensive, so make sure it's done in on-chip 
 -       shared memory (hence, the 256-element limit).
 -}
-- reduce = foldl
--sortAcc :: Elt a => Vector a -> Acc (Vector a)
--sortAcc k arr = 
    
--checkSorted :: Elt a => Acc (Vector a) -> Acc 
checkSorted arr = Acc.fold (&&*) (lift True) $ Acc.zipWith (<=*) arr' arr 
    where arr' = Acc.prescanl (\x -> id) (arr ! (index1 0)) arr

{- Do a quicksort
 -}
seqSort :: Acc (Vector a) -> Acc (Vector a)
seqSort arr = undefined
    where
        -- copy pivot across with scanl
        pivots = scanl const (arr ! 0) arr
        -- array of 0s/1s, where 0 means < pivot, and 1 means >= pivot
        flags = zipWith (\x y -> (x <* y) ? (0,1)) arr pivots
        -- use split to arrange numbers < pivot at the beginning, before
        -- numbers >= pivot
        pivotSort = split arr flags

seqSort' arr flags = undefined
    where
        -- array of pivot values copied across to their resp. segments
        pivotArr = fstA pairPivotsFlags
        -- set flags to 0 or 1, based on comparison to pivots in pivotArr
        flags = zipWith (\x y -> (x <* y) ? (0,1)) arr pivotArr
        pairArrFlags   = zip arr flags
        pairPivotsFlags = scanl1 f pairArrFlags
        -- (Int, flag) -> (Int, flag) -> (Int, flag)
        -- copy first value with a 1 flag across all 0 flags
        -- when a new 1 flag is found, copy that value (flags are unchanged)
        f = \prev next -> let (a, flag) = unlift next
                              (pa, pflag) = unlift prev in
                            if flag ==* 0
                            then (pa, flag)
                            else (a, flag)
        

-- sort arr based on flags, where flags is an array of 0's and 1's, and
-- numbers in arr will be sorted (stably) so that 0's will be at the beginning
-- and 1's at the end
split :: Acc (Vector a) -> Acc (Vector Bool) -> Acc (Vector a)
split arr flags = permute const arr (\i -> index1 (newInds ! i)) arr
    where
        -- choose from iup and idown, depending on whether flag is 0 or 1
        choose f x = let (a,b) = unlift x in (f ==* 0) ? (a,b)
        -- n = array size
        n     = constant $ (arraySize $ arrayShape arr) - 1
        -- indices for 0's (numbers < pivot)
        idown = prescanl (+) 0 . map (xor 1) $ flags
        -- indices for 1's (numbers >= pivot)
        iup   = map (n -) . prescanr (+) 0   $ flags
        -- calculate new indices, based on 0's and 1's
        newInds = zipWith choose flags (zip idown iup) 

xor 1 1 = 0
xor 0 1 = 1
xor 1 0 = 1
xor 0 0 = 0 

-- Scatter: takes an array of elements and an array of their destination 
--   indices. Uses Acc.permute to move each element from the original array to 
--   the specified index.
scatter :: Elt a => Acc (Vector Int) -> Acc (Vector a) -> Acc (Vector a)
scatter ind orig = Acc.permute const orig fetchInd orig
    where fetchInd x = index1 (ind ! x)


{-
-- I think we need Acc -> Acc type here, not Vec -> Acc because it's recursive
mergesort :: Acc (Vector Int) -> Acc (Vector Int)
mergesort xs = 
-}


-- Simple Accelerate example, not used at all
-- fold and zipWith are from Data.Array.Accelerate, not Prelude
dotp :: Vector Float -> Vector Float -> Acc (Scalar Float)
dotp xs ys = let xs' = use xs
                 ys' = use ys in
             fold (+) 0 (zipWith (*) xs' ys')


