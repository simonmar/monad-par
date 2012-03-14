{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -O2 #-}

-- Microbenchmark for parfib with pedigree.

import Data.Int
import qualified Data.List as L
import System.Environment
import GHC.Conc
import System.Random
import Control.Monad.Par.Class as PC

-- import qualified Control.Monad.Trans.State.Strict as TS
-- import Data.BitList

import Control.Monad.Par.Pedigree 

#ifdef PARSCHED 
import PARSCHED
#else
-- import Control.Monad.Par
--import Control.Monad.Par.Scheds.ContFree
import Control.Monad.Par.Scheds.Direct
#endif

-- Define a RNG layer on top of whatever Par implementation was imported above:
-- type MyPar a = TS.StateT Pedigree Par a

-- type FibType = Int64

-- Same recursion pattern as parfib but with extra cost at fork points
-- to split the RNG.
pedTest :: Int -> ParPedigreeT Par Int64
pedTest n | n < 2 = do
    p <- pedigree
    -- Here we only return 1 for the "rightmost" path:
    if all id (unpack p)
     then return 1 
     else return 0
pedTest n = do 
    xf <- PC.spawn_$ pedTest (n-1)
    y  <-            pedTest (n-2)
    x  <- PC.get xf
    return (x+y)


main = do 
    args <- getArgs
    let size = case args of 
            []      -> 20
            [n]     -> read n

--    let sum = runPar $ TS.evalStateT (pedTest size) empty 
    let sum = runPar $ runParPedigree (pedTest size)

    putStrLn$ "Pedigree test: "++ show sum

{-

[2011.11.29]

Timing RNG.hs:
 On a 3.1ghz 4 core intel westmere:

     threads input time-pedigree time-parfib
    1 34  6.7s   1.78s
    2 34  3.5s   0.96s
    4 34  1.9s   0.55s

That was using a "BitList" for pedigree.  Using a simple [Bool] does a
little better and yields the following:

    1 34  6.3s   1.78s
    2 34  3.3s   0.96s
    4 34  1.78s  0.55s

I'm seeing low productivities (a lot of time in GC).. typically 37%
with default settings and 52% with -A1M.

 -}
