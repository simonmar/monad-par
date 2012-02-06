{-# LANGUAGE CPP #-}

-- A pseq version of the quicksort monad-par benchmark

import System.Random
import System.Environment
import GHC.Conc
#ifdef PARSCHED 
import PARSCHED
#else
import Control.Monad.Par
#endif

quicksortS :: [Int] -> [Int]
quicksortS [] = []
quicksortS [x] = [x]
quicksortS (pivot:xs) = low `par` high `pseq` (low ++ [pivot] ++ high)
  where
    low = quicksortS $ filter (< pivot) xs
    high = quicksortS $ filter (< pivot) xs
    
genRandoms :: Int -> [Int]
genRandoms n = take n $ randoms (mkStdGen 120) :: [Int]

main = do args <- getArgs
          let size =
                case args of
                  []  -> 2^8
                  [n] -> (read n)
          
          let rands = genRandoms size
          putStrLn "Pseq based version:"
          print$ head $ quicksortS rands
  
  
    