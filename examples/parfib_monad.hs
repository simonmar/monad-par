{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -O2 #-}
import Data.Int
import System.Environment
import GHC.Conc
#ifdef PARSCHED 
import PARSCHED
#else
-- import Control.Monad.Par
--import Control.Monad.Par.Scheds.ContFree
import Control.Monad.Par.Scheds.Direct
#endif

type FibType = Int64

-- sequential version of the code
fib :: FibType -> FibType
fib 0 = 1
fib 1 = 1
fib x = fib (x-2) + fib (x-1)


-- Par monad version:
parfib1 :: FibType -> Par FibType
parfib1 n | n < 2 = return 1
parfib1 n = do 
--    xf <- spawn1_ parfib1 (n-1)
    xf <- spawn_$ parfib1 (n-1)
    y  <-         parfib1 (n-2)
    x  <- get xf
    return (x+y)

-- Par monad version, with threshold:
parfib1B :: FibType -> FibType -> Par FibType
parfib1B n c | n <= c = return $ fib n
parfib1B n c = do 
    xf <- spawn_$ parfib1B (n-1) c
    y  <-         parfib1B (n-2) c
    x  <- get xf
    return (x+y)

-- Gratuitously nested Par monad version:
parfibNest :: FibType -> FibType -> Par FibType
parfibNest n c | n <= c = return $ fib n
parfibNest n c = do 
    xf <- spawnP $ runPar $ helper (n-1) c
    yf <- spawnP $ runPar $ helper (n-2) c
    x  <- get xf
    y  <- get yf
    return (x+y)
 where 
  -- Alternate between nesting and regular spawning:
  helper :: FibType -> FibType -> Par FibType
  helper n c | n <= c = return $ fib n
  helper n c = do 
    xf <- spawn_$ parfibNest (n-1) c
    y  <-         parfibNest (n-2) c
    x  <- get xf
    return (x+y)


main = do 
    args <- getArgs
    let (version, size, cutoff) = case args of 
            []      -> ("monad", 20, 1)
            [v]     -> (v,       20, 1)
            [v,n]   -> (v, read n,   1)
            [v,n,c] -> (v, read n, read c)

    case version of 
        "nested" -> do 
                print$ runPar$ parfibNest size cutoff
        "monad"  -> 
		if cutoff == 1 
                then do putStrLn "Using non-thresholded version:"
                        print$ runPar$ parfib1  size 
		else    print$ runPar$ parfib1B size cutoff
        -- TEMP: force thresholded version even if cutoff==1
        "thresh" -> print$ runPar$ parfib1B size cutoff
        _        -> error$ "unknown version: "++version


{- 

[2011.03] On 4-core nehalem, 3.33ghz:
-------------------------------------

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

   1 thread: 17.53 -- e.g. 4.16X speedup


[2011.03.29] {A bit more measurement}
-------------------------------------

If I run a CnC/C++ parfib where results are (multiply) written into an
item collection (so there are many insertions, but only a small
number of resident items), then I get these numbers:

  fib(34) 1 thread: 22.78
  fib(34) 4 thread: 13.96 -- 1.63X 

ESTIMATED 3573.76 seconds for fib(42).


[2011.10.20] {ContFree approach}
--------------------------------

Initial version forks a new thread on every get, which does terribly of course.

    +RTS -N1 : 
     fib(24) 2.3s vs. 0.086


[2011.10.11] {Westmere 3.1GHz NoHT 4-core testing}
--------------------------------------------------

Testing schedulers directly, without going through the generic (type
class) interface.  Starting with Scheds.Sparks:
  user/system time: 

  fib(42) 4 threads: 4.56  17.83   -- Sparks
  fib(42) 4 threads: 50.0  191.6   -- Trace 


[2011.10.24] {Timing nested scheduler version} 
----------------------------------------------

Checking for performance regression.  This is on a 3.1 GHz Westmere
with hyperthreading disabled.  First a plain fib on the nested branch:

Data Schema:            User, system, productivity, alloc
    fib(38) 1 thread :   20.2  19.7   94.1%  82GB   -- TraceNested
    fib(38) 4 threads:   6.23  24.2   90.6%  85GB   -- TraceNested

And for arguments sake with a cutoff of 10:
    fib(42) 1 thread :   5.5   5.5    89.2%  8.2GB  -- TraceNested
    fib(42) 4 threads:   1.72  6.38   87.5%  8.4GB  -- TraceNested

And with the Sparks scheduler:
    fib(38) 1 thread :   2.2
    fib(38) 4 threads:   .75   2.7    69.0%  7.5GB
    fib(42) 1 thread :   14.8  14.5   82.8%  52GB
    fib(42) 4 threads:   4.7   18.3   71.1%  52GB
    fib(42) 4 threads:   1.0   3.8    100%   11MB -- cutoff 10

And the plain par/pseq version:
    fib(42) 1 thread :   8.7   8.6    86.2%  17GB 
    fib(42) 4 threads:   2.8   10.5   73.9%  17GB

And then for regression testing the ORIGINAL Trace scheduler (no nesting support):
    fib(38) 1 thread :   22.1  21.5   93.8%  97GB -- TraceOrig
    fib(38) 4 threads:   7.5   28.6   90.4%  97GB -- TraceOrig

Indeed, rather than regression, it would seem that Daniel improved the
parfib performance!

Super-nested parfib:
-----------------------
And the perversely Nested parfib:
    nfib(38) 1 thread :   3.3   3.2    82.7%  12G      -- nested but Sparks.hs
    nfib(38) 4 threads:   1.1   4.1    70.9%  12.9GB   -- nested but Sparks.hs

Oops!  That was with the sparks scheduler!  Here's the actual Trace/nested:    
    nfib(30) 4 threads:   1.3   4.8    93.5%  7GB    -- super nested fib / trace
    nfib(32) 4 threads:   3.26  11.7   92.9%  18GB
    nfib(42) 4 threads:   6.5   23.5   94.7%  29.6GB -- cutoff 10:
  (Note, those only used 376% cpu.)

Finally, this is the original Trace scheduler on the perversely nested parfib:

    nfib(30) 1 thread :   1.8   1.8    92.1%  5GB
    nfib(32) 1 threads:   4.9   4.8    91.7%  14.9GB
    nfib(32) 4 threads:   -- memory explosion
    nfib(28) 4 threads:   9.7   37.2   33.8%  5.8GB -- 2GB ram usage

One interesting consequence here is that while the Sparks scheduler
has an 8X advantage over Trace (and par/pseq an additional 60%
advantage, 13.8X total), that advantage widens to over 256X in the
case of the perversely nested parfib!!!

-}
