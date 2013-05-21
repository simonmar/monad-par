{-# OPTIONS_GHC -O2 #-}
import Data.Int
import System.Environment
import GHC.Conc
type FibType = Int64

-- sequential version of the code
fib :: FibType -> FibType
fib 0 = 1
fib 1 = 1
fib x = fib (x-2) + fib (x-1) + 1

-- Basic, non-monadic parallel-fib:
parfib0 :: FibType -> FibType
parfib0 n | n < 2 = 1 
parfib0 n = x `par` y `pseq` (x+y)
  where 
     x = parfib0 (n-1)
     y = parfib0 (n-2)  



main = do args <- getArgs	  
          let size = 
                   case args of 
		      []    -> 10
		      [n] -> read n

	  putStrLn "Par/pseq sparks version of parfib:"
	  print$ parfib0 size
