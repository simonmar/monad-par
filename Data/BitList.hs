
-- | A simple module that provides a more memory-efficient representation of `[Bool]`.

module Data.BitList 
  ( BitList
  , cons, head, tail, empty 
  , pack, unpack, length, drop
  )
where

import Data.Int
import Data.Bits
import Prelude as P hiding (head,tail,drop,length)
import qualified Data.List as L
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Gen

data BitList = One  {-# UNPACK #-} !Int {-# UNPACK #-} !Int64
             | More {-# UNPACK #-} !Int {-# UNPACK #-} !Int64 BitList
 
instance Show BitList where 
 show bl = "BitList " ++ show (map (\b -> case b of True -> '1'; False -> '0') (unpack bl))
-- show bl = "pack " ++ show (unpack bl)

empty :: BitList
empty = One 0 0

cons :: Bool -> BitList -> BitList
cons True  x@(One  64 _ )   = More 1 1 x
cons False x@(One  64 _ )   = More 1 0 x
cons True  x@(More 64 bv _) = More 1 1 x
cons False x@(More 64 bv _) = More 1 0 x
cons True    (One   i bv)   = One  (i+1) (bv `setBit` i)
cons False   (One   i bv)   = One  (i+1) (bv           )
cons True    (More  i bv r) = More (i+1) (bv `setBit` i) r
cons False   (More  i bv r) = More (i+1) (bv           ) r

-- TODO: May consider (More 0 _ _) representation to reduce extra
-- allocation when size of the BitList is fluctuating back and forth.

head :: BitList -> Bool
head (One  0 _   ) = error "tried to take head of an empty BitList"
head (More 0 _  r) = error "BitList: data structure invariant failure!"
head (One  i bv  ) = bv `testBit` (i-1)
head (More i bv r) = bv `testBit` (i-1)

tail :: BitList -> BitList
tail (One  0 _   ) = error "tried to take the tail of an empty BitList"
tail (One  i bv  ) = One  (i-1) bv
tail (More 1 bv r) = r
tail (More i bv r) = More (i-1) bv r

pack :: [Bool] -> BitList
pack  []   = One 0 0
pack (h:t) = cons h (pack t)

unpack :: BitList -> [Bool] 
unpack (One 0 _)     = []
unpack (One i bv)    = (bv `testBit` (i-1)) : unpack (One (i-1) bv)
unpack (More 0 _ r)  =  unpack r
unpack (More i bv r) = (bv `testBit` (i-1)) : unpack (More (i-1) bv r)

-- drop :: Int -> BitList -> BitList
-- drop 0 bl           = bl
-- drop n bl | n >= 64 = case bl of 
-- 		        One _ _    -> error "drop: not enough elements in BitList"
-- 			More i _ r -> drop (n-i) r
-- drop n bl = case bl of 
-- 	      One i  bv   -> One  (i-n) bv
-- 	      More i bv r -> More (i-n) bv r

drop :: Int -> BitList -> BitList
drop n (One i bv)
   | n >= i    = empty
   | otherwise = One (i - n) bv
drop n (More i bv r)
   | n >= i    = drop (n - i) r
   | otherwise = More (i - n) bv r

length :: BitList -> Int
length (One  i _)   = i
length (More i _ r) = i + length r


-- TODO: index, take, etc

-- TODO: functor instance, etc.

instance Eq BitList where
 One i1 bv1 == One i2 bv2 
    | i1 == i2 && mask bv1 == mask bv2  = True

mask x = x

--------------------------------------------------------------------------------
-- Testing:

t1 = pack (L.concat$ L.replicate 10 [True,False,True])

t2 = L.length $ unpack $ pack $ replicate 1000 True

t3 = pack $ replicate 1000 True
t4 = drop 500 t3
p3 = L.and (unpack t3)
p4 = L.and (unpack t4)

t5 = iterate tail t4 !! 250
t5a = length t5
t5b = L.length (unpack t5)

t6 = drop 5 (More 1 0 (One 64 0)) 
-- More (-4) 0 (One 64 0)

tests :: Test
tests = 
  TestList 
    [ 
      show t1 ~=? "BitList \"101101101101101101101101101101\""
    , t2  ~=? 1000
    , t5a ~=? 250
    , t5b ~=? 250
    , p3  ~=? True
    , p4  ~=? True
    , length t6 ~=? 60
    ]

-- TODO: QuickCheck

-- \s -> length (take 5 s) == 5

-- This won't work at tail []:
prop_droptail xs =   drop 1 xs == tail xs

q1 = quickCheck (prop_droptail :: BitList -> Bool)

instance Arbitrary BitList where
  arbitrary = MkGen $ \ rng n -> 
	        let ls = (unGen arbitrary) rng n
		in pack ls