{-# LANGUAGE TypeSynonymInstances #-}

-- | This module extends a Par monad with /pedigree/.  That is, it
--   allows a running computation to look up its position in the
--   dynamic binary tree of `fork` calls ("ancestry").

module Control.Monad.Par.Pedigree
 (
   pedigree
 ) 
 where 

import Data.BitList
-- import Control.Exception
import qualified Control.Monad.Par as Default
import Control.Monad.Par.Class
import Control.Monad.Par.State
-- import Control.Monad.Trans
import Control.Monad.Trans.State.Strict as S 

type Pedigree = BitList

-- class ParPedigree p where 
--  pedigree :: p Pedigree

-- | Trivial instance.
instance SplittableState Pedigree where
  splitState bl = (cons False bl, cons True bl)

pedigree :: ParFuture p iv => S.StateT Pedigree p Pedigree
pedigree = S.get
