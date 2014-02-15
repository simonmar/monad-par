{-# LANGUAGE DataKinds, Rank2Types #-}

-- | Test how much a (useless) StateT transformer screws up optimizations and adds
-- overheads, if at all.

module LVishPlusCancelT
       (
         runPar, Par,
         module Control.Par.Class 
       ) where

import qualified Control.LVish as L 
import Control.Par.Class
import Control.LVish.CancelT

type Par d s a = CancelT (L.Par d s) a

runPar :: (forall s . Par L.Det s a) -> a
runPar m =
  L.runPar $ 
  runCancelT m 
