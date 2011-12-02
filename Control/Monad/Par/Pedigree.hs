{-# LANGUAGE TypeSynonymInstances, CPP, FlexibleInstances #-}

-- | This module extends a Par monad with /pedigree/.  That is, it
--   allows a running computation to look up its position in the
--   dynamic binary tree of `fork` calls ("ancestry").

module Control.Monad.Par.Pedigree
 (
   pedigree, ParPedigreeT
 , unpack, runParPedigree
 ) 
 where 

import qualified Control.Monad.Par as Default
import Control.Monad.Par.Class
import Control.Monad.Par.State
import Control.Monad.Trans.State.Strict as S 
#if 0 
import Data.BitList
type Pedigree = BitList
#else
type Pedigree = [Bool]
unpack x = x
cons = (:)
empty = []
#endif
type ParPedigreeT p a = S.StateT Pedigree p a

-- | Trivial instance.
instance SplittableState Pedigree where
  splitState bl = (cons False bl, cons True bl)

pedigree :: ParFuture p iv => S.StateT Pedigree p Pedigree
pedigree = S.get

runParPedigree :: Monad p => ParPedigreeT p a -> p a
runParPedigree m = S.evalStateT m empty
