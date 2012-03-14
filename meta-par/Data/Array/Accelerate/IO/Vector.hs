{-# LANGUAGE CPP, TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | Helpers for fast conversion of 'Data.Vector.Storable' vectors
-- into Accelerate arrays.
module Data.Array.Accelerate.IO.Vector where

import Data.Array.Accelerate ( arrayShape
                             , Array
                             , DIM1
                             , Elt
                             , Z(..)
                             , (:.)(..))
import Data.Array.Accelerate.Array.Sugar (EltRepr)
import Data.Array.Accelerate.IO
import Data.Vector.Storable ( fromList, unsafeFromForeignPtr0
                            , unsafeToForeignPtr0
                            , Vector)

import Foreign (mallocForeignPtrArray, Ptr, Storable, withForeignPtr)

import System.IO.Unsafe

#define TEST
#ifdef TEST
import Data.Int
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Monadic
#endif

fromVector :: (Storable a, Elt a, BlockPtrs (EltRepr a) ~ ((), Ptr a))
           => Vector a -> IO (Array DIM1 a)
fromVector v = withForeignPtr fp $ \ptr -> fromPtr (Z :. len) ((), ptr)
  where (fp, len) = unsafeToForeignPtr0 v

toVector :: (Storable a, Elt a, BlockPtrs (EltRepr a) ~ ((), Ptr a)) 
         => Array DIM1 a -> IO (Vector a)
toVector arr = do
  let (Z :. len) = arrayShape arr
  fp <- mallocForeignPtrArray len
  withForeignPtr fp $ \ptr -> toPtr arr ((), ptr)
  return $ unsafeFromForeignPtr0 fp len

unsafeFromVector :: (Storable a, Elt a, BlockPtrs (EltRepr a) ~ ((), Ptr a))
                 => Vector a -> Array DIM1 a
unsafeFromVector v = unsafePerformIO $ fromVector v

unsafeToVector :: (Storable a, Elt a, BlockPtrs (EltRepr a) ~ ((), Ptr a)) 
         => Array DIM1 a -> Vector a
unsafeToVector arr = unsafePerformIO $ toVector arr

#ifdef TEST
prop_roundtrip :: ( Arbitrary a
                  , Eq a
                  , Elt a
                  , Storable a
                  , BlockPtrs (EltRepr a) ~ ((), Ptr a) )
               => [a] -> Property
prop_roundtrip xs = monadicIO $ do
  let xsv = fromList xs
  xsarr <- run $ fromVector xsv
  xsv'  <- run $ toVector xsarr
  assert (xsv == xsv')

prop_unsaferoundtrip :: ( Arbitrary a
                        , Eq a
                        , Elt a
                        , Storable a
                        , BlockPtrs (EltRepr a) ~ ((), Ptr a) )
                       => [a] -> Bool
prop_unsaferoundtrip xs = xsv == (unsafeToVector (unsafeFromVector xsv))
  where xsv = fromList xs

{- OK, not so much of a pain in the butt; they just need to be monomorphic.

*Data.Array.Accelerate.IO.Vector> quickCheck (prop_roundtrip :: [Double] -> Property)
+++ OK, passed 100 tests.

-}

#endif