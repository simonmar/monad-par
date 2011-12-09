{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -O2 #-}

-- Note: this is really a test/microbenchmark, not an example.  Should
-- move it.

import Data.Int
import System.Environment
import GHC.Conc
import System.Random
import Control.Monad.Par.RNG
import Control.Monad.Par.Class as PC
import qualified Control.Monad.Trans.State.Strict as TS
-- import qualified Control.Monad.Par.RNG_direct as Old

#ifdef PARSCHED 
import PARSCHED
#else
-- import Control.Monad.Par
--import Control.Monad.Par.Scheds.ContFree
import Control.Monad.Par.Scheds.Direct
#endif

-- Define a RNG layer on top of whatever Par implementation was imported above:
type MyPar a = TS.StateT StdGen Par a

-- type FibType = Int64

-- Same recursion pattern as parfib but with extra cost at fork points
-- to split the RNG.
parRand :: Int -> MyPar Int
parRand n | n < 2 = randInt
parRand n = do 
    xf <- PC.spawn_$ parRand (n-1)
    y  <-            parRand (n-2)
    x  <- PC.get xf
    return (x+y)

-- This generates randoms on the way down as well as the leaves.
parRand2 :: Int -> MyPar Int
parRand2 n | n < 2 = randInt
parRand2 n = do 
    extra <- randInt
    xf <- PC.spawn_$ parRand2 (n-1)
    y  <-            parRand2 (n-2)
    x  <- PC.get xf
    return (x+y+extra)


-- parRand3 :: Int -> Old.Par Int
-- parRand3 n | n < 2 = Old.randInt
-- parRand3 n = do 
--     extra <- Old.randInt
--     xf <- Old.spawn_$ parRand3 (n-1)
--     y  <-             parRand3 (n-2)
--     x  <- Old.get xf
--     return (x+y+extra)


-- This version maintains RNG state but doesn't actually generate randoms.
parConst :: Int -> MyPar Int
parConst n | n < 2 = return 1
parConst n = do 
    xf <- PC.spawn_$ parConst (n-1)
    y  <-            parConst (n-2)
    x  <- PC.get xf
    return (x+y)

main = do 
    args <- getArgs
    let (version,size) = case args of 
            []      -> ("rand",20)
            [n]     -> ("rand",read n)
	    [ver,n] -> (ver   ,read n)

--    runParRNG$ parRand size 
    sum <- case version of 
            "const" -> runParRand runPar $ parConst size 
            "rand"  -> runParRand runPar $ parRand  size 
            "rand2" -> runParRand runPar $ parRand2 size 
--            "rand3" -> do g <- newStdGen
--                          return (Old.runParRNG g $ parRand3 size)

    putStrLn$ "Sum of ~2^"++show size++" random ints: "++ show sum


{-

[2011.11.29]

Timing RNG.hs:
 On a 3.1ghz 4 core intel westmere:

  threads input time-parRand time-parfib
    1 34  8.3s   1.78s
    2 34  4.4s   0.96s
    4 34  2.4s   0.55s

And the Const version:

    1 34  6.2s
    2 34  3.3s
    4 34  1.7s

And the Rand2 version:

    1 34  10.0
    2 34  5.3s
    4 34  2.8s

For arguments sake I tested the old version as well (rand3):

    1 34  8.3
    2 34  4.8
    4 34  2.6s
    
  (That was based on Trace, with Direct its a smidge better: 2.49s.)

So we're seeing about 5X overhead for splitting RNGs and for actually
generating randoms (with not-super-efficient inefficient default
System.Random).

BUT, we haven't made the RNG state strict yet.

Doing that yields a very slight improvement.  Using mode "rand":

    1 34  7.99s
    2 34  4.23s
    4 34  2.27s

HOWEVER - that's getting 37% productivity on only 4 threads!!!!

 -}
