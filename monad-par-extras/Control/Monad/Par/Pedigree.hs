{-# LANGUAGE TypeSynonymInstances, CPP, FlexibleInstances, BangPatterns #-}

-- | This module extends a Par monad with /pedigree/.  That is, it
--   allows a running computation to look up its position in the
--   dynamic binary tree of `fork` calls ("ancestry").

module Control.Monad.Par.Pedigree
 (
   pedigree, ParPedigreeT
 , unpack, runParPedigree
 ) 
 where 

import Control.Monad.Par.Class
import Control.Monad.Par.State
import Control.Monad.Trans.State.Strict as S 

-- It's running slightly better with normal lists for parfib:
#if 0 
import Data.BitList
type BList = BitList
#else
type BList = [Bool]
unpack (Pedigree _ x) = x
cons = (:)
empty = []
#endif

type ParPedigreeT p a = S.StateT Pedigree p a

-- type Pedigree = BList
-- -- | Trivial instance.
-- instance SplittableState Pedigree where
--   splitState bl = (cons False bl, cons True bl)

data Pedigree = 
      Pedigree { ivarCounter :: {-# UNPACK #-} !Int, 
	         treePath    :: !BList }

instance SplittableState Pedigree where
  splitState (Pedigree cnt bl) = 
    (Pedigree cnt (cons False bl), 
     Pedigree cnt (cons True bl))

pedigree :: ParFuture iv p => S.StateT Pedigree p Pedigree
pedigree = S.get

runParPedigree :: Monad p => ParPedigreeT p a -> p a
runParPedigree m = S.evalStateT m (Pedigree 0 empty)
