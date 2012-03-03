{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}

-- #define TEST
#ifndef TEST
module Data.Vector.Algorithms.CUDA.Merge ( unsafeMergeSort ) where
#endif

import Control.Applicative
import Control.Monad

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

import Data.Word (Word32)

import Foreign.C
import Foreign.CUDA
import qualified Foreign.Marshal as Marsh

#ifdef TEST
import System.Random.MWC
#endif

foreign import ccall "initMergeSort" cu_initMergeSort :: IO () 
foreign import ccall "closeMergeSort" cu_closeMergeSort :: IO ()

foreign import ccall "mergeSort" cu_mergeSort 
  :: DevicePtr CUInt -- ^ dstKey
  -> DevicePtr CUInt -- ^ dstVal
  -> DevicePtr CUInt -- ^ bufKey
  -> DevicePtr CUInt -- ^ bufVal
  -> DevicePtr CUInt -- ^ srcKey
  -> DevicePtr CUInt -- ^ srcVal
  -> CUInt           -- ^ N
  -> CUInt           -- ^ sortDir
  -> IO ()

mergeSortKeysVals :: M.IOVector CUInt
                  -> M.IOVector CUInt
                  -> Int
                  -> CUInt
                  -> IO (M.IOVector CUInt, M.IOVector CUInt)
mergeSortKeysVals srcKey srcVal n dir = do
  when (n `mod` 1024 /= 0) $ error "mergeSort: vector length must be multiple of 1024"
  M.unsafeWith srcKey $ \skPtr ->
    M.unsafeWith srcVal $ \svPtr -> 
      allocaArray n $ \skDevPtr ->
      allocaArray n $ \svDevPtr ->
      allocaArray n $ \bkDevPtr ->
      allocaArray n $ \bvDevPtr ->
      allocaArray n $ \dkDevPtr ->
      allocaArray n $ \dvDevPtr -> do
        kHostPtr <- mallocHostArray [DeviceMapped] n 
        vHostPtr <- mallocHostArray [DeviceMapped] n
        withHostPtr kHostPtr $ \ptr -> Marsh.copyArray ptr skPtr n
        withHostPtr vHostPtr $ \ptr -> Marsh.copyArray ptr svPtr n
        pokeArrayAsync n kHostPtr skDevPtr Nothing
        pokeArrayAsync n vHostPtr svDevPtr Nothing
        cu_initMergeSort
        sync
        cu_mergeSort dkDevPtr dvDevPtr bkDevPtr bvDevPtr skDevPtr svDevPtr (fromIntegral n) dir
        sync
        peekArray n dkDevPtr skPtr
        peekArray n dvDevPtr svPtr
        cu_closeMergeSort
        freeHost kHostPtr
        freeHost vHostPtr
        return (srcKey, srcVal)

-- | Unsafely sort a 'Vector' of 'Word32's (corresponding to 'CUInt's)
-- in place. The original vector may no longer be used after this.
unsafeMergeSort :: V.Vector Word32 -> IO (V.Vector Word32)
unsafeMergeSort v = do
  let n = V.length v
  mv <- M.unsafeCast <$> V.unsafeThaw v
  mv2 <- M.new n
  (mv', _) <- mergeSortKeysVals mv mv2 n 1
  V.unsafeFreeze $ M.unsafeCast mv'


#ifdef TEST
test :: IO (V.Vector Word32)
test = do
  sk <- srcKey
  unsafeMergeSort sk

srcKey :: IO (V.Vector Word32)
srcKey = withSystemRandom $ \g -> (uniformVector g (fromIntegral n) :: IO (V.Vector Word32))

n :: Int
n = floor (2**20)

sortDir :: CUInt
sortDir = 1

main = do print =<< test
#endif