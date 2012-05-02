

-- What is the overhead of spawning a single operation on the GPU?
-- In this microbenchmark we measure it.

import Control.Monad.Par.Accelerate
import Control.Monad.Par.Meta.AccSMP 

-- import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc

import qualified Data.Array.Accelerate.IO as IO
-- import qualified Data.Vector as V


-- Dot product
-- -----------
dotpAcc :: Vector Float -> Vector Float -> Acc (Scalar Float)
dotpAcc xs ys
  = let
      xs' = use xs
      ys' = use ys
    in
    Acc.fold (+) 0 (Acc.zipWith (*) xs' ys')


main = do 
  print "hi"
  
  let v1 = listArray (0,1) [3.3] :: UArray Int Float
      v2 = listArray (0,1) [4.4] :: UArray Int Float
--  v1' <- convertUArray v1
--  v2' <- convertUArray v2
  
  print $ runPar $ do
--    spawnAcc (dotpAcc (V.singleton 3.3) (V.singleton 4.4))
    
--     spawnAcc (dotpAcc (listArray (0,1) [3.3]) 
--                       (listArray (0,1) [4.4]))
    return 33
