{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Interpreter (run)

import qualified Data.Vector as V
import Prelude hiding (map, zipWith)
import qualified Prelude as P

-- Hello 

-- foo arr = 
  

-- runfoo = foo (fromIArray (V.replicate 10 10))
foo = do 
  putStrLn$ "indices0: " ++ show (run indices0)
  putStrLn$ "samples0: " ++ show (run samples0)
  putStrLn$ "vals0: "    ++ show (run vals0)
  putStrLn$ "bools0: "   ++ show (run bools0)

  putStrLn$ "indices1: " ++ show (run indices1)
--   foldSeg (+) 0 (use arr) (use segs)
  where 

    arr :: Acc (Array DIM1 Int)
    arr  = use$ fromList ((Z :. 512) :: DIM1 ) [1..512::Int]
--    arr = fromList ((Z :. 512) :: DIM1 ) (P.map (\_->0::Int) [1..512::Int])
    segs = fromList ((Z :. 2) :: DIM1 ) (P.replicate 2 (256::Int))

--    indices0 = use$ fromList ((Z :. 2) :: DIM1 ) (P.replicate 2 (0::Int))
    indices0 = use$ fromList ((Z :. 2) :: DIM1 ) [0,256::Int]
    vals0    = gather indices0 arr
    -- Pretend this is what we're looking for:
    samples0 = use$ fromList ((Z :. 2) :: DIM1 ) [112,256::Int]

    bools0   = zipWith (<*) vals0 samples0

    indices1 = zipWith (maybeStride 128) bools0 indices0

    maybeStride incr bool n = bool ? (n+incr, n)

    -- Example of how to do a loop in the metaprogram to generate code
    -- for 8 stages of binary searching:
    _ = loop 256 indices0

    loop 1 indices = undefined
    loop stride indices = loop (stride `quot` 2) arr


--    indices = fromList (index1 2) (P.replicate 2 (0::Int))

-- gather
--   :: (Shape ix, Shape ix1, Elt b) =>
--      Acc (Array ix ix1) -> Acc (Array ix1 b) -> Acc (Array ix b)

-- gather :: Acc (Array DIM1 Int) -> Acc (Array DIM1 a) -> Acc (Array DIM1 a)
gather inds arr = 
   Acc.map (\ i -> arr ! (index1 i)) inds
