{-# LANGUAGE DataKinds, Rank2Types #-}

-- | Test how much a (useless) StateT transformer screws up optimizations and adds
-- overheads, if at all.

module LVishPlusStateT
       (
         runPar, Par,
         module Control.Par.Class 
       ) where

import qualified Control.LVish as L 
import Control.Par.Class
import Control.Par.StateT
import qualified Control.Monad.State.Strict as S

type Par d s a = S.StateT () (L.Par d s) a

runPar :: (forall s . Par L.Det s a) -> a
runPar m =
  L.runPar $ 
  S.evalStateT m ()

instance SplittableState () where
  splitState () = ((),())
