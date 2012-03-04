{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
module Main where

import Control.Monad
-- TODO: switch
import qualified Data.Vector.Unboxed as V 
import qualified Data.Vector.Unboxed.Mutable as MV

import System.Random
import System.Environment
import Control.Exception
import Test.QuickCheck (Arbitrary, arbitrary, sized, choose, vector)

import Data.List.Split (chunk)
import Data.List (intersperse)

import Data.Time.Clock
import Text.Printf
import Data.Vector.Algorithms.Intro (sort)

#ifdef PARSCHED 
import PARSCHED
#else
import Control.Monad.Par (runPar, spawn_, get, Par)
#endif


-- import Random.MWC.Pure (seed, range_random)

sortVec v = V.create $ do 
                mut <- V.thaw v
                sort mut
                return mut

-- Merge sort for a Vector using the Par monad
-- t is the threshold for using sequential merge (see merge)
mergesort :: Int -> V.Vector Int -> Par (V.Vector Int)
mergesort t vec = if (V.length vec) <= t
                  then return $ sortVec vec
                  else do
                      let n = (V.length vec) `div` 2
                      let (lhalf, rhalf) = V.splitAt n vec
                      ileft <- spawn_ (mergesort t lhalf)
                      right <-         mergesort t rhalf
                      left  <- get ileft
                      merge t left right

-- If either list has length less than t, use sequential merge. Otherwise:
-- 1. Find the median of the two combined lists using findSplit
-- 2. Split the lists at the median
-- 3. Merge each of the lefts and rights
-- 4. Append the merged lefts and the merged rights
merge :: Int -> (V.Vector Int) -> (V.Vector Int) -> Par (V.Vector Int)
merge t left right =
        if ((V.length left) < t) || ((V.length right) < t)
        then return $ seqmerge left right
        else do
            let (splitL, splitR) = findSplit left right
            let (llhalf, rlhalf) = V.splitAt splitL left
            let (lrhalf, rrhalf) = V.splitAt splitR right
            isortLeft <- spawn_ (merge t llhalf lrhalf)
            sortRight <-         merge t rlhalf rrhalf
            sortLeft  <- get isortLeft
            return (sortLeft V.++ sortRight)
        
{-
 - Given two sorted vectors, return a pair of indices such that splitting on 
 - these indices results in 4 vectors in which every value in the two left 
 - side vectors is smaller than all of the values in the right side vectors.
 -
 - In other words, if lIndex and rIndex are the splitting points for the
 - vectors named left and right, then every item in left, up to lIndex, is
 - smaller than every item in right from rIndex on; and every item in right,
 - up to rIndex, is smaller than every item in left from lIndex on.
 -
 - Additionally, (lIndex + rIndex) should be as close to 
 - (length(left) + length(right))/2 as possible.
 -}
findSplit :: V.Vector Int -> V.Vector Int -> (Int, Int)
findSplit left right = (lIndex, rIndex)
        where
            (lIndex, rIndex) = split 0 (V.length left) 0 (V.length right)

            split :: Int -> Int -> Int -> Int -> (Int, Int)
            split lLow lHigh rLow rHigh = 
                let lIndex = (lLow + lHigh) `div` 2
                    rIndex = (rLow + rHigh) `div` 2 in
                    
                    if (lIndex == 0)
                    then if (right V.! (rIndex - 1)) < (left V.! lIndex)
                         then (lIndex, rIndex)
                         else split 0 0 rLow rIndex
                    else if (rIndex == 0)
                    then if (left V.! (lIndex - 1)) < (right V.! rIndex)
                         then (lIndex, rIndex)
                         else split lLow lIndex 0 0
                    else if (left V.! (lIndex - 1)) < (right V.! rIndex) &&
                            (right V.! (rIndex - 1)) < (left V.! lIndex)
                    then (lIndex, rIndex)
                    else if (left V.! (lIndex - 1)) < (right V.! rIndex)
                    then split lIndex lHigh rLow rIndex
                    else split lLow lIndex rIndex rHigh

-- Sequential merge: takes two sorted vectors and merges them in a sequential
-- fashion.
-- Although vector cons is supported, it requires O(n) time. Since list cons
-- is much faster, we'll build up a list of tuples and use the batch update
-- for vectors: (//).
seqmerge :: V.Vector Int -> V.Vector Int -> V.Vector Int
seqmerge left right = 
    -- (left V.++ right) V.// (seqhelp 0 left right)
    V.unsafeUpd (V.replicate len 0) (seqhelp 0 left right)

    where
        len = (V.length left) + (V.length right)
        seqhelp :: Int -> V.Vector Int -> V.Vector Int -> [(Int, Int)]
        seqhelp n left right = 
            if n >= len
            then []
            else if V.null left
            then zip [n..(n + V.length right)] (V.toList right)
            else if V.null right
            then zip [n..(n + V.length left)]  (V.toList left)
            else if (V.head left) < (V.head right)
            then (n, V.head left)  : seqhelp (n+1) (V.tail left) right
            else (n, V.head right) : seqhelp (n+1) left (V.tail right)

-- Create a vector containing the numbers [0,N) in random order.
randomPermutation :: Int -> StdGen -> V.Vector Int
randomPermutation len rng = 
  -- Annoyingly there is no MV.generate:
  V.create (do v <- V.unsafeThaw$ V.generate len id
               loop 0 v rng)
  -- loop 0 (MV.generate len id)
 where 
  loop n vec g | n == len  = return vec
	       | otherwise = do 
    let (offset,g') = randomR (0, len - n - 1) g
--    MV.unsafeSwap vec n 
    MV.swap vec n (n + offset)
    loop (n+1) vec g'


commaint :: (Show a, Integral a) => a -> String
commaint n | n < 0 = "-" ++ commaint (-n)
commaint n = 
   reverse $ concat $
   intersperse "," $ 
   chunk 3 $ reverse (show n)


-- Main, based on quicksort main
-- Usage: ./Main [expt] [threshold]
-- t is threshold to bottom out to sequential sort and sequential merge
-- expt controls the length of the vector to sort (length = 2^expt)
main = do args <- getArgs
          let (expt, t) = case args of
                            -- The default size should be very small.
                            -- Just for testing, not for benchmarking:
                            []  -> (10, 2)
                            [n] -> (read n, 2)
                            [n, t] -> (read n, read t)

          g <- getStdGen

          putStrLn $ "Merge sorting " ++ commaint (2^expt) ++ 
                     " elements. First generate a random permutation:"

          start <- getCurrentTime
          let rands = randomPermutation (2^expt) g
          evaluate$ rands
          evaluate$ rands V.! 0
          end   <- getCurrentTime
          printf "Creating vector took %0.3f sec.\n"
            ((fromRational$ toRational $ diffUTCTime end start) :: Double)

          putStrLn "Executing monad-par based sort..."
          start <- getCurrentTime
          let sorted = runPar $ mergesort t rands
          putStr "Prefix of sorted list:\n  "
          print $ V.slice 0 8 sorted
          end   <- getCurrentTime
          let runningTime = ((fromRational $ toRational $ diffUTCTime end start) :: Double)
          printf "Sorting vector took %0.3f sec.\n" runningTime
          putStrLn $ "SELFTIMED " ++ show runningTime
          when (expt <= 4) $ do
            putStrLn$ "  Unsorted: " ++  show rands
            putStrLn$ "  Sorted  : " ++  show sorted


-- Needed for Par monad to work with unboxed vectors
-- instance NFData (V.Vector Int) where
--   rnf = rnf . V.toList
-- 
-- RRN: This ^^ is very inefficient!  If you did want to force the
-- evaluation, at the very worst you would want to do a fold to
-- traverse a (boxed) vector, but with an unboxed vector the whole
-- thing is evaluated when one element is.  It should be enough to
-- read one element.  (It *might* be enough to just use "pseq" or
-- "evaluate" on the vector value itself, but I'm not sure.)


-- Used for QuickCheck
instance Arbitrary (V.Vector Int) where
    arbitrary = do
        ls <- sized (\n -> choose (0, n) >>= vector)
        return (V.fromList ls)
