{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

--
-- Radix sort for a subclass of element types. Taken and modified
-- slightly from the Accelerate benchmark suite.
--

import qualified Prelude
import Prelude               hiding (zip, map, scanl, scanr, zipWith, fst)
import Data.Bits             hiding (shiftL, shiftR, bit, testBit)
import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.CUDA as CUDA

import Control.Exception     (evaluate)
import Data.Array.IArray     hiding ((!))
import Data.Array.Unboxed    (UArray)
import Data.Array.IO         (MArray, IOUArray)
import qualified Data.Array.MArray as M

import Data.List             (sort)
import Data.Array.Unboxed    (IArray, UArray, listArray, bounds, elems)
import System.Environment    (getArgs, withArgs)
import System.Random
import System.Random.MWC
import Unsafe.Coerce

-- import Criterion.Main

-- import Control.Monad.Par.Meta.SharedMemoryAccelerate
import Foreign.CUDA (sync)

-- Radix sort
-- ----------

class Elt e => Radix e where
    passes :: Exp e   -> Int                -- Haskell-side control needs to know this
    radix  :: Exp Int -> Exp e -> Exp Int

instance Radix Int where                    -- may be 32- or 64-bit
    passes    = bitSize . (undefined :: Exp t -> t)
    radix i e = i ==* (passes' e - 1) ? (radix' (e `xor` minBound), radix' e)
      where
        radix' x = (x `shiftR` i) .&. 1
        passes'  = constant . passes

-- For IEEE-754 floating-point representation. Unsafe, but widely supported.
--
instance Radix Float where
    passes _   = 32
    radix i e  = let x = (unsafeCoerce e :: Exp Int32)
                 in  i ==* 31 ? (radix' (x `xor` minBound), radix' (floatFlip x))
      where
        floatFlip x = x `testBit` 31 ? (complement x, x)  -- twos-complement negative numbers
        radix'    x = x `testBit` i  ? (1,0)


--
-- A simple (parallel) radix sort implementation [1].
--
-- [1] G. E. Blelloch. "Prefix sums and their applications." Technical Report
--     CMU-CS-90-190. Carnegie Mellon University. 1990.
--
sortAcc :: Radix a => Vector a -> Acc (Vector a)
sortAcc = sortAccBy id

sortAccBy :: (Elt a, Radix r) => (Exp a -> Exp r) -> Vector a -> Acc (Vector a)
sortAccBy rdx arr = foldr1 (>->) (Prelude.map radixPass [0..p-1]) (use arr)
  where
    n = constant $ (arraySize $ arrayShape arr) - 1
    p = passes . rdx . (undefined :: Vector e -> Exp e) $ arr

    deal f x      = let (a,b) = unlift x in (f ==* 0) ? (a,b)
    radixPass k v = let flags = map (radix (constant k) . rdx) v
                        idown = prescanl (+) 0 . map (xor 1) $ flags
                        iup   = map (n-) . prescanr (+) 0    $ flags
                        index = zipWith deal flags (zip idown iup)
                    in
                    permute const v (\ix -> index1 (index!ix)) v


sortRef :: UArray Int Int -> UArray Int Int
sortRef xs = listArray (bounds xs) $ sort (elems xs)

-- Convert an Unboxed Data.Array to an Accelerate Array
--
convertUArray :: (IArray UArray e, Acc.Elt e) => UArray Int e -> IO (Acc.Vector e)
convertUArray v =
  let arr = Acc.fromIArray v
  in  evaluate (arr `Acc.indexArray` (Z:.0)) >> return arr

-- Generate a random, uniformly distributed vector of specified size over the
-- range. For integral types the range is inclusive, for floating point numbers
-- the range (a,b] is used, if one ignores rounding errors.
--
randomUArrayR :: (Variate a, MArray IOUArray a IO, IArray UArray a)
  => (a,a)
  -> GenIO
  -> Int
  -> IO (UArray Int a)
randomUArrayR lim gen n = do
  mu  <- M.newArray_ (0,n-1) :: MArray IOUArray e IO => IO (IOUArray Int e)
  let go !i | i < n     = uniformR lim gen >>= M.writeArray mu i >> go (i+1)
            | otherwise = M.unsafeFreeze mu
  go 0

-- Main
-- ----

main :: IO () -- Int -> IO (() -> UArray Int Int, () -> Acc (Vector Int))
main = withSystemRandom $ \gen -> do
  args <- getArgs
  let (n,args') = case args of 
                    []  -> (10000 :: Int, [])
                    (n:args') -> ((read n), args')
  
  vec  <- randomUArrayR (minBound,maxBound) gen n
  vec' <- convertUArray vec
  --
--  sync
--  withArgs args' $ defaultMain $ [bench "radix" $ whnf CUDA.run $ sortAcc (vec' :: Vector Int)]
  print $ CUDA.run (sortAcc (vec' :: Vector Int))
  where
    {-# NOINLINE run_ref #-}
    run_ref xs () = sortRef xs
    run_acc xs () = sortAcc xs
--    run_par xs () = runPar $ do
--      ans <- spawnAcc $ run_acc (xs :: Vector Int) ()
--      get ans