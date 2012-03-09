{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | 'Data.Serialize' instance for 'Data.Vector' vectors, given that
-- they contain an instance of 'Data.Serialize' themselves. The
-- serialized format is an 'Int64' representing the length of the
-- 'Vector', followed by the serialized contents of each element.
module Data.Vector.Serialize where

import Control.Monad (when)

import Data.Int (Int64)
import Data.Serialize (Get, encode, decode, Serialize(..))
import Data.Vector (Vector)
import qualified Data.Vector as V

#define TEST
#ifdef TEST
import Test.QuickCheck
#endif

instance Serialize a => Serialize (Vector a) where
  get = do 
    len64 <- (get :: Get Int64)
    when (len64 > fromIntegral (maxBound :: Int)) $
      fail "Host can't deserialize a Vector longer than (maxBound :: Int)"
    let len    = fromIntegral len64
    V.replicateM len get

  put v = do
    put $ ((fromIntegral $ V.length v) :: Int64)
    V.mapM_ put v

#ifdef TEST
prop_roundtrip :: (Arbitrary a, Eq a, Serialize a) => [a] -> Bool
prop_roundtrip xs = (Right xsv) == decode (encode xsv)
  where xsv = V.fromList xs

prop_roundtripTuple :: --( Arbitrary a, Eq a, Storable a
                       --, Arbitrary b, Eq b, Storable b ) =>
                       [[Int]]
                    -> [Either [Bool] Double]
                    -> Bool
prop_roundtripTuple xs ys = (Right (xsv, ysv)) == decode (encode (xsv, ysv))
  where (xsv, ysv) = (V.fromList xs, V.fromList ys)
#endif