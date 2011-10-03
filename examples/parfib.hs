import Data.Int
import System.Environment
import Control.Monad.Par
import GHC.Conc

type FibType = Int64

-- sequential version of the code
fib :: FibType -> FibType
fib 0 = 1
fib 1 = 1
fib x = fib (x-2) + fib (x-1)


-- Basic, non-monadic parallel-fib with threshold:
parfib0 :: FibType -> FibType -> FibType
parfib0 n c | n < c = fib n
parfib0 n c = x `par` y `pseq` (x+y)
  where 
    x = parfib0 (n-1) c
    y = parfib0 (n-2) c


-- Par monad version:
parfib1 :: FibType -> FibType -> Par FibType
parfib1 n c | n < c = return $ fib n
parfib1 n c = do 
    xf <- spawn_$ parfib1 (n-1) c
    y  <-         parfib1 (n-2) c
    x  <- get xf
    return (x+y)

-- Gratuitously nested Par monad version:
parfib2 :: FibType -> FibType -> Par FibType
parfib2 n c | n < c = return $ fib n
parfib2 n c = do 
    xf <- pval $ runPar $ parfib3 (n-1) c
    yf <- pval $ runPar $ parfib3 (n-2) c
    x  <- get xf
    y  <- get yf
    return (x+y)
 where 
  -- Alternate between nesting and regular spawning:
  parfib3 :: FibType -> FibType -> Par FibType
  parfib3 n c | n < c = return $ fib n
  parfib3 n c = do 
    xf <- spawn_$ parfib2 (n-1) c
    y  <-         parfib2 (n-2) c
    x  <- get xf
    return (x+y)



main = do 
    args <- getArgs
    let (version, size, cutoff) = case args of 
            []      -> ("monad", 20, 4)
            [v,n,c] -> (v, read n, read c)

    case version of 
        "nested" -> do 
                print$ runPar$ parfib2 size cutoff
        "monad"  -> do 
                print$ runPar$ parfib1 size cutoff
        "sparks" -> do 
                putStrLn "Sparks-based, Non-monadic version:"
                print$ parfib0 size cutoff
        _        -> error$ "unknown version: "++version


{- 

[2011.03] On 4-core nehalem, 3.33ghz:

  Non-monadic version, real/user time:
  fib(40) 4 threads: 1.1s 4.4s
  fib(42) 1 threads: 9.7s  
  fib(42) 4 threads: 2.86s 11.6s  17GB allocated -- 3.39X
  
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

   1 thread: 17.53 -- 4.16X

[2011.03.29] {A bit more measurement}


If I run a CnC/C++ parfib where results are (multiply) written into an
item collection (so there are many insertions, but only a small
number of resident items), then I get these numbers:

  fib(34) 1 thread: 22.78
  fib(34) 4 thread: 13.96 -- 1.63X 

ESTIMATED 3573.76 seconds for fib(42).

-}
