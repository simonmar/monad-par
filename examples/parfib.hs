
import Data.Int
import System.Environment
import Control.Monad.Par
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



parfib1 :: FibType -> Par FibType
parfib1 n | n < 2 = return 1 
parfib1 n = 
  do 
     xf <- spawn_$ parfib1 (n-1)
     y  <-         parfib1 (n-2)  
     x  <- get xf
     return (x+y)

main = do args <- getArgs	  
          let (version,size) = 
                   case args of 
		      []    -> ("monad",10)
		      [v,n] -> (v,read n)

	  case version of 
	    "monad"  -> do 
			   putStrLn "Monad-par based version:"
			   putStr$ "fib("++show size++") = "++show (runPar$ parfib1 size)
	    "sparks" -> do 
           	           putStrLn "Sparks-based, Non-monadic version:"
			   putStr$ "fib("++show size++") = "++ show(parfib0 size)
	    _        -> error$ "unknown version: "++version


{- On 4-core nehalem, 3.33ghz:

  Non-monadic version, real/user time:
  fib(40) 4 threads: 1.1s 4.4s
  fib(42) 4 threads: 2.9s 11.6s  17GB allocated
  
     SPARKS: 433784785 (290 converted, 280395620 pruned)

  Monad-par version:
  fib(38) non-threaded: 23.3s 23.1s
  fib(38) 1 thread :    24.7s 24.5s
  fib(38) 4 threads:     8.2s 31.3s

  fib(40) 4 threads:    20.6s 78.6s 240GB allocated


For comparison, Cilkarts Cilk++:
  fib(42) 4 threads:  3.029s 23.610s

Intel Cilk Plus:
  fib(42) 4 threads:  4.212s 16.770s

-}