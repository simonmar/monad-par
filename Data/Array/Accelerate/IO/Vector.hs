{-# LANGUAGE CPP, ScopedTypeVariables, TypeFamilies #-}
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

fromVector :: forall a e. (Storable a, Elt e, BlockPtrs (EltRepr e) ~ ((), Ptr a))
           => Vector a -> IO (Array DIM1 e)
fromVector v = withForeignPtr fp $ \ptr -> fromPtr (Z :. len) ((), ptr)
  where (fp, len) = unsafeToForeignPtr0 v

toVector :: (Storable a, Elt e, BlockPtrs (EltRepr e) ~ ((), Ptr a)) 
         => Array DIM1 e -> IO (Vector a)
toVector arr = do
  let (Z :. len) = arrayShape arr
  fp <- mallocForeignPtrArray len
  withForeignPtr fp $ \ptr -> toPtr arr ((), ptr)
  return $ unsafeFromForeignPtr0 fp len

test :: IO (Array DIM1 Float)
test = fromVector $ fromList ([1,2] :: [Float])

#ifdef TEST
prop_roundtrip :: forall a e. ( Arbitrary a
                              , Eq a
                              , Elt e
                              , Storable a
                              , BlockPtrs (EltRepr e) ~ ((), Ptr a) )
               => Array DIM1 e -> [a] -> Property
prop_roundtrip (_ :: Array DIM1 e) xs = monadicIO $ do
  let xsv = fromList xs
  xsarr <- run $ fromVector xsv
  xsv'  <- run $ toVector (xsarr :: Array DIM1 e)
  assert (xsv == xsv')

{- WOW. These things are a serious pain in the ass to use. Seems like
   you have to provide a witness of the equivalence at each use;
   gross.

*Data.Array.Accelerate.IO.Vector> quickCheck (prop_roundtrip (undefined :: Array DIM1 Double) :: [Double] -> Property)
+++ OK, passed 100 tests.

-}

#endif