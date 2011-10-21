{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fwarn-unused-imports #-}

-- | This module defines the 'AList' type, a list that supports
-- constant-time append, and is therefore ideal for building the
-- result of tree-shaped parallel computations.

module Control.Monad.Par.AList
 (
  -- * The 'AList' type and operations
  AList(..),
  empty, singleton, cons, head, tail, length, null, append,
  toList, fromList,
  -- * Operations to build 'AList's in the 'Par' monad
  parBuildThresh, parBuildThreshM,
  parBuild, parBuildM,
 )
where 


import Control.DeepSeq
import Prelude hiding (length,head,tail,null)
import qualified Prelude as P
import Control.Monad.Par.Class
import qualified Control.Monad.Par.Combinator as C

-- | List that support constant-time append (sometimes called
-- join-lists).
data AList a = ANil | ASing a | Append (AList a) (AList a) | AList [a]

instance NFData a => NFData (AList a) where
 rnf ANil         = ()
 rnf (ASing a)    = rnf a 
 rnf (Append l r) = rnf l `seq` rnf r
 rnf (AList  l)   = rnf l

instance Show a => Show (AList a) where 
  show al = "fromList "++ show (toList al)

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

-- TODO: Provide a strategy for @par@-based maps:


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
parBuildThresh :: (NFData a, ParFuture p f) => Int -> C.InclusiveRange -> (Int -> a) -> p (AList a)
parBuildThresh threshold range fn =
  C.parMapReduceRangeThresh threshold range
			  (return . singleton . fn) appendM empty

-- | Variant of 'parBuildThresh' in which the element-construction function is itself a 'Par' computation.
parBuildThreshM :: (NFData a, ParFuture p f) => Int -> C.InclusiveRange -> (Int -> p a) -> p (AList a)
parBuildThreshM threshold range fn =
  C.parMapReduceRangeThresh threshold range 
			  (\x -> fn x >>= return . singleton) appendM empty

-- | \"Auto-partitioning\" version of 'parBuildThresh' that chooses the threshold based on
--    the size of the range and the number of processors..
parBuild :: (NFData a, ParFuture p f) => C.InclusiveRange -> (Int -> a) -> p (AList a)
parBuild range fn =
  C.parMapReduceRange range (return . singleton . fn) appendM empty

-- | like 'parBuild', but the construction function is monadic
parBuildM :: (NFData a, ParFuture p f) => C.InclusiveRange -> (Int -> p a) -> p (AList a)
parBuildM range fn =
  C.parMapReduceRange range (\x -> fn x >>= return . singleton) appendM empty


--------------------------------------------------------------------------------
-- Internal helpers:

appendM :: ParFuture p f => AList a -> AList a -> p (AList a)
appendM x y = return (append x y)

--------------------------------------------------------------------------------


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

#if 0
 data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
 instance Traversable Tree
	traverse f Empty = pure Empty
	traverse f (Leaf x) = Leaf <$> f x
	traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
#endif

