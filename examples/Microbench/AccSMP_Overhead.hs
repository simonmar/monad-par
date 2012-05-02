

-- What is the overhead of spawning a single operation on the GPU?
-- In this microbenchmark we measure it.

import Control.Monad.Par.Accelerate
import Control.Monad.Par.Meta.AccSMP (runPar)

-- import System.Random.MWC
import Data.Array.Unboxed
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z, (:.))

import qualified Data.Array.Accelerate.IO as IO
-- import qualified Data.Vector as V

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Exception (evaluate)

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
  
  let 
      v1 = A.fromList (A.Z A.:. (5::Int)) [1..5::Float]
      v2 = A.fromList (A.Z A.:. (5::Int)) [6..10::Float]
  
  start <- getPOSIXTime
  x <- evaluate$ runPar $ spawnAcc (dotpAcc v1 v2)
  end <- getPOSIXTime
  putStrLn$ "First execution took: "++show (end-start)
