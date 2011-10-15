{-# LANGUAGE CPP #-}

-- A quicksort benchmark for monad-par

import System.Random
import System.Environment
#ifdef PARSCHED 
import PARSCHED
#else
import Control.Monad.Par
#endif

-- TODO: Rewrite with AList.. lists are not good for this.

quicksortP :: [Int] -> Par [Int]
quicksortP [] = return []
quicksortP [x] = return [x]
-- This quicksort always choses the pivot to be the first element:
quicksortP (pivot:xs) = 
  do
    let low  =  filter (<  pivot) xs
        high =  filter (>= pivot) xs
    lf <- spawn$ quicksortP low
    h  <-        quicksortP high
    l  <- get lf

    -- This O(N) append is serial:
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