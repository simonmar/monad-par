{-# LANGUAGE CPP #-}

-- A quicksort benchmark for monad-par

import System.Random
import System.Environment
#ifdef PARSCHED 
import PARSCHED
#else
import Control.Monad.Par
#endif


quicksortP :: [Int] -> Par [Int]
quicksortP [] = return []
quicksortP [x] = return [x]
quicksortP (pivot:xs) = 
  do
    let low = (filter (< pivot) xs)
        high = (filter (>= pivot) xs)
    lf <- spawn$ quicksortP low
    h <-         quicksortP high
    l <- get lf
    return (l ++ [pivot] ++ h)
    
genRandoms :: Int -> [Int]
genRandoms n = take n $ randoms (mkStdGen 120) :: [Int]

main = do args <- getArgs
          let size =
                case args of
                  [] -> 25000
                  [n] -> (read n)
          
          let rands = genRandoms size
          putStrLn "Monad-par based version:"
          print$ head $ runPar$ quicksortP rands