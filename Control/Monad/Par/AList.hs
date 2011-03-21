{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fwarn-unused-imports #-}

-- Do these really not already have an implementation on hackage?
module Control.Monad.Par.AList 
 (
  AList(..), 
  empty, singleton, cons, head, tail, length, null, append,
  toList, fromList,
--  parMapM, 
  parBuildThresh, parBuildThreshM,
  parBuild, parBuildM,
  alist_tests
 )
where 


import Control.Applicative hiding (empty)
import Control.DeepSeq
import Test.HUnit
import Prelude hiding (length,head,tail,null)
import qualified Prelude as P
import Control.Monad.Par
import Data.Traversable
import qualified Data.Foldable  as F 

-- | A datatype for append-based lists that are cheap to construct
--  (and to convert from plain lists).
data AList a = ANil | ASing a | Append (AList a) (AList a) | AList [a]

instance NFData a => NFData (AList a) where
 rnf ANil         = ()
 rnf (ASing a)    = rnf a 
 rnf (Append l r) = rnf l `seq` rnf r
 rnf (AList  l)   = rnf l

#if 0
 data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
 instance Traversable Tree
	traverse f Empty = pure Empty
	traverse f (Leaf x) = Leaf <$> f x
	traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
#endif


-- TODO: Finish me:
-- instance F.Foldable AList where
--  foldr fn init al = 
--   case al of 
--    ANil    -> 

-- instance Functor AList where
--  fmap = undefined

-- -- Walk the data structure without introducing any additional data-parallelism.
-- instance Traversable AList where 
--   traverse f al = 
--     case al of 
--       ANil    -> pure ANil
--       ASing x -> ASing <$> f x

instance Show a => Show (AList a) where 
  show al = "fromList "++ show (toList al)

append :: AList a -> AList a -> AList a
append ANil r = r
append l ANil = l -- **
append l r    = Append l r

{-# INLINE empty #-}
{-# INLINE singleton #-}
{-# INLINE fromList #-}

empty     ::        AList a
singleton ::  a  -> AList a
fromList  :: [a] -> AList a

empty     = ANil
singleton = ASing
fromList  = AList

-- If we tracked length perhaps this could make an effort at balance.
cons :: a -> AList a -> AList a 
cons x ANil = ASing x
cons x al   = Append (ASing x) al

-- Alas there are an infinite number of representations for null:
head :: AList a -> a 
head al = 
  case loop al of
    Just x -> x 
    Nothing -> error "cannot take head of an empty AList"
 where 
  loop al =
   case al of 
     Append l r -> case loop l of 
		     x@(Just _) -> x
		     Nothing    -> loop r
     ASing x     -> Just x
     AList (h:_) -> Just h
     AList []    -> Nothing
     ANil        -> Nothing

tail :: AList a -> AList a 
tail al = 
  case loop al of
    Just x -> x 
    Nothing -> error "cannot take tail of an empty AList"
 where 
  loop al =
   case al of 
     Append l r -> case loop l of 
		     (Just x) -> Just (Append x r)
		     Nothing  -> loop r

     ASing x     -> Just ANil
     AList (_:t) -> Just (AList t)
     AList []    -> Nothing
     ANil        -> Nothing

length :: AList a -> Int
length ANil         = 0
length (ASing _)    = 1
length (Append l r) = length l + length r
length (AList  l)   = P.length l 


null :: AList a -> Bool
null = (==0) . length 


toList :: AList a -> [a]
toList a = go a []
 where go ANil         rest = rest
       go (ASing a)    rest = a : rest
       go (Append l r) rest = go l $! go r rest
       go (AList xs)   rest = xs ++ rest

-- TODO: Provide a strategy for @par@-based maps:


appendM x y = return (append x y)

-- | Build a balance AList in parallel constructing each element as a
--   function of its index.  The threshold argument provides control
--   over the degree of parallelism.  It indicates under what number
--   of elements the build process should switch from parallel to
--   serial.
parBuildThresh :: NFData a => Int -> InclusiveRange -> (Int -> a) -> Par (AList a)
parBuildThresh threshold range fn =
  parMapReduceRangeThresh threshold range
			  (return . singleton . fn) appendM empty

-- | Variant in which the element-construction function is itself a Par computation.
parBuildThreshM :: NFData a => Int -> InclusiveRange -> (Int -> Par a) -> Par (AList a)
parBuildThreshM threshold range fn =
  parMapReduceRangeThresh threshold range 
			  ((fmap singleton) . fn) appendM empty

-- | "Auto-partitioning" version that chooses the threshold based on
--    the size of the range and the number of processors..
parBuild :: NFData a => InclusiveRange -> (Int -> a) -> Par (AList a)
parBuild range fn =
  parMapReduceRange range (return . singleton . fn) appendM empty

-- | Auto-partitioning plus monadic construction function.
parBuildM :: NFData a => InclusiveRange -> (Int -> Par a) -> Par (AList a)
parBuildM range fn =
  parMapReduceRange range ((fmap singleton) . fn) appendM empty


-- | A parMap over an AList can result in more balanced parallelism than
--   the default parMap over Traversable data types.
-- parMap :: NFData b => (a -> b) -> AList a -> Par (AList b)


--------------------------------------------------------------------------------
-- Testing

-- For testing:
bintree 0 x = x
bintree n x = Append sub sub
 where sub = bintree (n-1) x

showDbg ANil         = "_"
showDbg (ASing x)    = show x
showDbg (Append l r) = "("++showDbg l++" | "++showDbg r++")"
showDbg (AList  l)   = show l

alist_tests :: Test
alist_tests = 
  TestList 
    [

      8   ~=? (length$ tail$ tail$ fromList [1..10])
    , 1   ~=? (length$ tail$tail$  cons 1$ cons 2$ cons 3 empty)

    , 253 ~=? (length$ tail$tail$tail$ bintree 8 $ singleton 'a')
    , 0   ~=? (length$ bintree 8 $ empty)

    , "((1 | 1) | (1 | 1))" ~=? (showDbg$            bintree 2 $ singleton 1)
    , "((_ | 1) | (1 | 1))" ~=? (showDbg$ tail$      bintree 2 $ singleton 1)
    , "(_ | (1 | 1))"       ~=? (showDbg$ tail$tail$ bintree 2 $ singleton 1)

    ]

t = runTestTT alist_tests


-- TODO: Quickcheck.