
import Control.Monad
import Control.Monad.Par
import qualified Data.Vector.Unboxed as V 
import qualified Data.Vector.Unboxed.Mutable as MV

import System.Random
import System.Environment
import Control.Exception
import Test.QuickCheck
import Control.DeepSeq (NFData(..), deepseq)
import Control.Parallel.Strategies (rdeepseq, runEval)

import Data.List.Split (chunk)
import Data.List (intersperse)
import System.CPUTime (getCPUTime)
import Text.Printf


import qualified Data.List as L

import qualified Data.Vector.Algorithms.Intro as VA

-- Haskell standard library
sort_list1 l = L.sort l


-- vector-algorithms package which uses a similar algorithm as the C++ STL sort:
sort_vector2 v = 
  V.create $ do mut <- V.thaw v 
                VA.sort mut
                return mut

-- main = do args <- getArgs
--           let (t, size) = case args of
--                             [] -> (2, 18)
--                             [t] -> ((read t), 18)
--                             [t, n] -> ((read t), (read n))

--           g <- getStdGen
--           let rands = genRandoms size g

--           putStrLn $ "Merge sorting " ++ show (V.length rands) ++ 
--                      " elements. First deepseq the rands."
--           --evaluate (deepseq rands ())


--           putStrLn "Monad-par based version:"
--           print $ take 8 $ V.toList $ runPar $ mergesort t rands



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


vecSortMain sorter = do 
     args <- getArgs
     let (t, exponent) = case args of
		       []  -> (2, 18)
		       [t] -> (read t, 18)
		       [t, n] -> (read t, read n)

     g <- getStdGen

     putStrLn $ "Sorting " ++ commaint (2^exponent) ++ 
		" elements. First generate a random permutation:"

     start <- getCPUTime
     let rands = randomPermutation (2^exponent) g
     evaluate$ rands
     evaluate$ rands V.! 0
     end   <- getCPUTime
     printf "Creating vector took %0.3f sec.\n"
	    ((fromIntegral$ end - start) / (10^12) :: Double)

     putStrLn "Executing monad-par based sort..."
     start <- getCPUTime
     let sorted = sorter rands
     putStr "Prefix of sorted list:\n  "
     print $ V.slice 0 8 sorted
     end   <- getCPUTime
     printf "Sorting vector took %0.3f sec.\n" 
	    ((fromIntegral$ end - start) / (10^12) :: Double)

--     printf "Checksum: "

     when (exponent <= 4) $ do
       putStrLn$ "  Unsorted: " ++  show rands
       putStrLn$ "  Sorted  : " ++  show sorted


main = vecSortMain sort_vector2
