{-# OPTIONS_GHC -XFlexibleInstances #-}

import Control.Monad.Par
import qualified Data.Vector.Unboxed as V (Vector, null, length, head, tail, 
            splitAt, cons, (!), (++), fromList, toList, singleton)
import System.Random
import System.Environment
import Control.Exception
import Test.QuickCheck
import Control.DeepSeq (NFData(..), deepseq)
import Control.Parallel.Strategies (rdeepseq, runEval)


-- Merge sort for a Vector using the Par monad
-- t is the threshold for using sequential merge (see merge)
mergesort :: Int -> V.Vector Int -> Par (V.Vector Int)
mergesort t vec = if (V.length vec) <= 1
                  then return vec
                  else do
                      let n = (V.length vec) `div` 2
                      let (lhalf, rhalf) = V.splitAt n vec
                      ileft <- spawn (mergesort t lhalf)
                      right <- mergesort t rhalf
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
        --else if ((V.length left) < t) || ((V.length right) < t)
        --then if (V.length left) < (V.length right)
        --     then return $ insertAll left right
        --     else return $  insertAll right left
        else do
            let (splitL, splitR) = findSplit left right
            let (llhalf, rlhalf) = V.splitAt splitL left
            let (lrhalf, rrhalf) = V.splitAt splitR right
            isortLeft <- spawn (merge t llhalf lrhalf)
            sortRight <- merge t rlhalf rrhalf
            sortLeft  <- get isortLeft
            return (sortLeft V.++ sortRight)
        where 
            insertAll :: (V.Vector Int) -> (V.Vector Int) -> V.Vector Int
            insertAll left right = 
                if V.null left 
                then right
                else insertAll (V.tail left) (insert (V.head left) right)

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
findSplit :: (V.Vector Int) -> (V.Vector Int) -> (Int, Int)
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
seqmerge :: (V.Vector Int) -> (V.Vector Int) -> (V.Vector Int)
seqmerge left right = if V.null left
                      then right
                      else if V.null right
                      then left
                      else if (V.head left) < (V.head right)
                      then V.cons (V.head left)
                                  (seqmerge (V.tail left) right)
                      else V.cons (V.head right)
                                  (seqmerge left (V.tail right))

-- Binary insertion: do a binary search to find where to insert the item
insert :: Int -> V.Vector Int -> V.Vector Int
insert i vec = if V.null vec
               then V.cons i vec
               else binInsert 0 (V.length vec)
    where
        binInsert :: Int -> Int -> V.Vector Int
        binInsert low high = 
            if mid == 0
            then if i < V.head vec
                 then V.cons i vec
                 else V.cons (V.head vec) (V.cons i (V.tail vec))
            else if mid == low
            then let (left, right) = V.splitAt (mid + 1) vec in
                    left V.++ (V.cons i right)
            else if (vec V.! (mid - 1)) < i
            then if i <= (vec V.! mid)
                 then let (left, right) = V.splitAt mid vec in
                        left V.++ (V.cons i right)
                 else binInsert mid high
            else binInsert low mid

            where mid = (low + high) `div` 2


-- Generate a Vector of random Ints
genRandoms :: Int -> StdGen -> V.Vector Int
genRandoms n g = loop g n
    where
      loop rng 0 = V.singleton $ fst $ next rng
      loop rng n = let (r1, r2) = split rng in
                     (V.++) (loop r1 (n-1))
                            (loop r2 (n-1))

-- Main, based on quicksort main
main = do args <- getArgs
          let (t, size) = case args of
                            [] -> (2, 18)
                            [t] -> ((read t), 18)
                            [t, n] -> ((read t), (read n))

          g <- getStdGen
          let rands = genRandoms size g

          putStrLn $ "Merge sorting " ++ show (V.length rands) ++ 
                     " elements. First deepseq the rands."
          --evaluate (deepseq rands ())


          putStrLn "Monad-par based version:"
          print $ take 8 $ V.toList $ runPar $ mergesort t rands

-- Needed for Par monad to work with unboxed vectors
instance NFData (V.Vector Int) where
  rnf = rnf . V.toList

-- Used for QuickCheck
instance Arbitrary (V.Vector Int) where
    arbitrary = do
        ls <- sized (\n -> choose (0, n) >>= vector)
        return (V.fromList ls)
