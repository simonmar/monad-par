{-# LANGUAGE CPP #-}

-- What is the overhead of spawning a single operation on the GPU?
-- In this microbenchmark we measure it.

import Control.Monad.Par.Accelerate
import Control.Monad.Par.Meta.AccSMP (runPar, get)

-- import System.Random.MWC
import Data.Array.Unboxed
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z, (:.))

import qualified Data.Array.Accelerate.IO as IO

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Exception (evaluate)

#ifdef USECUDA
#warning USING REAL CUDA
import Foreign.CUDA.Driver.Device (initialise)
import qualified Data.Array.Accelerate.CUDA        as Run
#else
import qualified Data.Array.Accelerate.Interpreter as Run
#endif

--------------------------------------------------------------------------------
-- Helpers:


--------------------------------------------------------------------------------

-- Dot product
-- -----------
dotpAcc :: A.Vector Float -> A.Vector Float -> A.Acc (A.Scalar Float)
dotpAcc xs ys
  = let
      xs' = A.use xs
      ys' = A.use ys
    in
    A.fold (+) 0 (A.zipWith (*) xs' ys')

main = do 
  putStrLn "Measuring one roundtrip through the GPU:"
  
#ifdef USECUDA
  start <- getPOSIXTime
  initialise []
  end <- getPOSIXTime
  putStrLn$ "GPU Initialization took "++show (end-start)
#endif

  let 
      v1 = A.fromList (A.Z A.:. (5::Int)) [1..5::Float]
      v2 = A.fromList (A.Z A.:. (5::Int)) [6..10::Float]
      acc = dotpAcc v1 v2    
      
      runone = do     
        start <- getPOSIXTime
        x <- evaluate$ runPar $ runAccWith Run.run acc 
        putStrLn$ "Result "++show x
        end <- getPOSIXTime
        return (end-start)

  x<-runone; putStrLn$ "First execution took: "++show x

  ls <- sequence (replicate 10 runone)
  putStrLn$ "Next ten took: "++show (sum ls)
