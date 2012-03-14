{-# LANGUAGE CPP #-}

-- A quicksort benchmark for monad-par

import Control.Monad
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Algorithms.Intro (sort)

import Data.List.Split (chunk)
import Data.List (intersperse)

import System.Random
import System.Environment
import Data.Time.Clock
import Text.Printf
import Control.Exception


#ifdef PARSCHED 
import PARSCHED
#else
import Control.Monad.Par
-- import Control.Monad.Par.Scheds.Trace
#endif

quicksortP :: Int -> V.Vector Int -> Par (V.Vector Int)
quicksortP t vec  = if V.length vec <= t
                    then return $ seqSort vec
                    else
                    -- just use the head as the pivot
                    let pivot = V.head vec
                        rest  = V.tail vec in
                    -- since we're using unboxed Ints, unstable partition should be fine
                    let (low, high) = V.unstablePartition (< pivot) vec in
                    do
                      lefti <- spawn_ $ quicksortP t low
                      right <-          quicksortP t high
                      left  <- get lefti
                      return $ left V.++ right

seqSort :: V.Vector Int -> V.Vector Int
seqSort vec = V.create $ do 
                mut <- V.thaw vec
                sort mut
                return mut

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


-- Main, based on mergesort main
-- Usage: ./Main [size_expt] [t]
-- t is the threshold for doing sequential sort
-- expt controls the length of the vector to sort (length = 2^expt)
main = do args <- getArgs
          let (size, t) = case args of
                            []  -> (18, 2)
                            [n] -> (read n, 2)
                            [n, t] -> (read n, read t)

          g <- getStdGen

          putStrLn $ "Quick sorting " ++ commaint (2^size) ++
                     " elements with threshold " ++ show t ++ 
                     ". First generate a random permutation: "

          start <- getCurrentTime
          let rands = randomPermutation (2^size) g
          evaluate $ rands
          evaluate $ rands V.! 0
          end   <- getCurrentTime
          printf "Creating vector took %0.3f sec. \n"
            ((fromRational $ toRational $ diffUTCTime end start) :: Double)

          putStrLn "Executing monad-par based sort..."
          start <- getCurrentTime
          let sorted = runPar $ quicksortP t rands
          putStr "Prefix of sorted list:\n  "
          print $ V.slice 0 8 sorted
          end   <- getCurrentTime

          let runningTime = ((fromRational $ toRational $ diffUTCTime end start) :: Double)
          printf "Sorting vector took %0.3f sec.\n" runningTime
          putStrLn $ "SELFTIMED " ++ show runningTime
          when (size <= 4) $ do
            putStrLn$ "  Unsorted: " ++  show rands
            putStrLn$ "  Sorted  : " ++  show sorted

--prop_aqs :: [Int] -> Bool
--prop_aqs xs = L.sort xs == (A.toList $ aqs (A.fromListBalanced xs))
