{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fwarn-unused-imports #-}

-- | This module defines the 'AList' type, a list that supports
-- constant-time append, and is therefore ideal for building the
-- result of tree-shaped parallel computations.

module Control.Monad.Par.AList 
{-# DEPRECATED "This structure does not perform well, and will be removed in future versions" #-}
 (
  -- * The 'AList' type and operations
  AList(..),
  empty, singleton, cons, head, tail, length, null, append,
  toList, fromList, fromListBalanced, 

  -- * Regular (non-parallel) Combinators
  filter, map, partition,

  -- * Operations to build 'AList's in the 'Par' monad
  parBuildThresh, parBuildThreshM,
  parBuild, parBuildM,

  -- * Inspect and modify the internal structure of an AList tree 
  depth, balance
 )
where 

import Control.DeepSeq
import Prelude hiding (length,head,tail,null,map,filter)
import qualified Prelude as P
import qualified Data.List as L
import qualified Control.Monad.Par.Combinator as C
import Control.Monad.Par.Class
import Data.Typeable
import qualified Data.Serialize as S

----------------------------------------------------------------------------------------------------

-- | List that support constant-time append (sometimes called
-- join-lists).
data AList a = ANil | ASing a | Append (AList a) (AList a) | AList [a]
 deriving (Typeable)

-- TODO -- Add vectors.

instance NFData a => NFData (AList a) where
 rnf ANil         = ()
 rnf (ASing a)    = rnf a 
 rnf (Append l r) = rnf l `seq` rnf r
 rnf (AList  l)   = rnf l

instance Show a => Show (AList a) where 
  show al = "fromList "++ show (toList al)

-- TODO: Better Serialization
instance S.Serialize a => S.Serialize (AList a) where
  put al = S.put (toList al)
  get = do x <- S.get 
	   return (fromList x)



----------------------------------------------------------------------------------------------------

{-# INLINE append #-}
-- | /O(1)/ Append two 'AList's
append :: AList a -> AList a -> AList a
append ANil r = r
append l ANil = l
append l r    = Append l r

{-# INLINE empty #-}
-- | /O(1)/ an empty 'AList'
empty :: AList a
empty = ANil

{-# INLINE singleton #-}
-- | /O(1)/ a singleton 'AList'
singleton :: a -> AList a
singleton = ASing

{-# INLINE fromList #-}
-- | /O(1)/ convert an ordinary list to an 'AList'
fromList :: [a] -> AList a
fromList  = AList

-- | Convert an ordinary list, but do so using 'Append' and
-- 'ASing' rather than 'AList'
fromListBalanced :: [a] -> AList a
fromListBalanced xs = go xs (P.length xs)
  where 
   go _  0 = ANil
   go ls 1 = case ls of 
	       (h:_) -> ASing h
	       []    -> error "the impossible happened"
   go ls n = 
     let (q,r) = quotRem n 2 in
     Append (go ls q)
            (go (drop q ls) (q+r))


-- | Balance the tree representation of an AList.  
balance :: AList a -> AList a
balance = fromListBalanced . toList
-- This would be much better if ALists tracked their size.

{-# INLINE cons #-}
-- | /O(1)/ prepend an element
cons :: a -> AList a -> AList a
cons x ANil = ASing x
cons x al   = Append (ASing x) al
-- If we tracked length perhaps this could make an effort at balance.

-- | /O(n)/ take the head element of an 'AList'
--
-- NB. linear-time, because the list might look like this:
--
-- > (((... `append` a) `append` b) `append` c)
--
head :: AList a -> a
head al = 
  case loop al of
    Just x -> x 
    Nothing -> error "cannot take head of an empty AList"
 where 
  -- Alas there are an infinite number of representations for null:
  loop al =
   case al of 
     Append l r -> case loop l of 
		     x@(Just _) -> x
		     Nothing    -> loop r
     ASing x     -> Just x
     AList (h:_) -> Just h
     AList []    -> Nothing
     ANil        -> Nothing

-- | /O(n)/ take the tail element of an 'AList'
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

     ASing _     -> Just ANil
     AList (_:t) -> Just (AList t)
     AList []    -> Nothing
     ANil        -> Nothing

-- | /O(n)/ find the length of an 'AList'
length :: AList a -> Int
length ANil         = 0
length (ASing _)    = 1
length (Append l r) = length l + length r
length (AList  l)   = P.length l 

{-# INLINE null #-}
-- | /O(n)/ returns 'True' if the 'AList' is empty
null :: AList a -> Bool
null = (==0) . length 

-- | /O(n)/ converts an 'AList' to an ordinary list
toList :: AList a -> [a]
toList a = go a []
 where go ANil         rest = rest
       go (ASing a)    rest = a : rest
       go (Append l r) rest = go l $! go r rest
       go (AList xs)   rest = xs ++ rest

partition :: (a -> Bool) -> AList a -> (AList a, AList a)
partition p a = go a (ANil, ANil)
  where go ANil      acc = acc
        go (ASing a) (ys, ns) | p a = (a `cons` ys, ns)
        go (ASing a) (ys, ns) | otherwise = (ys, a `cons` ns)
        go (Append l r) acc = go l $! go r acc
        go (AList xs) (ys, ns) = (AList ys' `append` ys, AList ns' `append` ns)
          where
            (ys', ns') = L.partition p xs

depth :: AList a -> Int
depth ANil      = 0
depth (ASing _) = 1
depth (AList _) = 1
depth (Append l r) = 1 + max (depth l) (depth r)


-- The filter operation compacts dead space in the tree that would be
-- left by ANil nodes.
filter :: (a -> Bool) -> AList a -> AList a
filter p l = loop l 
 where 
  loop ANil         = ANil
  loop o@(ASing x)  = if p x then o else ANil
  loop   (AList ls) = AList$ P.filter p ls
  loop (Append x y) = 
     let l = loop x
	 r = loop y in
     case (l,r) of 
       (ANil,ANil) -> ANil
       (ANil,y)    -> y
       (x,ANil)    -> x
       (x,y)       -> Append x y

-- | The usual `map` operation.
map :: (a -> b) -> AList a -> AList b
map _  ANil = ANil 
map f (ASing x) = ASing (f x)
map f (AList l) = AList (P.map f l)
map f (Append x y) = Append (map f x) (map f y)


--------------------------------------------------------------------------------
-- * Combinators built on top of a Par monad.

-- | A parMap over an AList can result in more balanced parallelism than
--   the default parMap over Traversable data types.
-- parMap :: NFData b => (a -> b) -> AList a -> Par (AList b)

-- | Build a balanced 'AList' in parallel, constructing each element as a
--   function of its index.  The threshold argument provides control
--   over the degree of parallelism.  It indicates under what number
--   of elements the build process should switch from parallel to
--   serial.
parBuildThresh :: (NFData a, ParFuture f p) => Int -> C.InclusiveRange -> (Int -> a) -> p (AList a)
parBuildThresh threshold range fn =
  C.parMapReduceRangeThresh threshold range
			  (return . singleton . fn) appendM empty

-- | Variant of 'parBuildThresh' in which the element-construction function is itself a 'Par' computation.
parBuildThreshM :: (NFData a, ParFuture f p) => Int -> C.InclusiveRange -> (Int -> p a) -> p (AList a)
parBuildThreshM threshold range fn =
  C.parMapReduceRangeThresh threshold range 
			  (\x -> fn x >>= return . singleton) appendM empty

-- | \"Auto-partitioning\" version of 'parBuildThresh' that chooses the threshold based on
--    the size of the range and the number of processors..
parBuild :: (NFData a, ParFuture f p) => C.InclusiveRange -> (Int -> a) -> p (AList a)
parBuild range fn =
  C.parMapReduceRange range (return . singleton . fn) appendM empty

-- | like 'parBuild', but the construction function is monadic
parBuildM :: (NFData a, ParFuture f p) => C.InclusiveRange -> (Int -> p a) -> p (AList a)
parBuildM range fn =
  C.parMapReduceRange range (\x -> fn x >>= return . singleton) appendM empty

--------------------------------------------------------------------------------

-- TODO: Provide a strategy for @par@-based maps:

-- TODO: tryHead -- returns Maybe

-- TODO: headTail -- returns head and tail, 
--    i.e. if we're doing O(N) work, don't do it twice.

-- FIXME: Could be more efficient:
instance Eq a => Eq (AList a) where
 a == b = toList a == toList b 

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


--------------------------------------------------------------------------------
-- Internal helpers:

appendM :: ParFuture f p => AList a -> AList a -> p (AList a)
appendM x y = return (append x y)
