{-# LANGUAGE DataKinds, Rank2Types, ScopedTypeVariables #-}

-- | Test how much a (useless) StateT transformer screws up optimizations and adds
-- overheads, if at all.

module LVishPlusStateT
       (
         runPar, Par,
         module Control.Par.Class 
       ) where

import qualified Control.LVish as L 
import Control.Par.Class
import Control.Par.ST
import qualified Control.Monad.State.Strict as S

type Par d s1 s0 a = ParST (STUnit s1) (L.Par d s0) a

runPar :: forall a . (forall s0 s1 . Par L.Det s1 s0 a) -> a
runPar m =
   L.runPar $
   runParST STUnit m
