{-# LANGUAGE DataKinds, Rank2Types, ScopedTypeVariables #-}

-- | Test how much a (useless) StateT transformer screws up optimizations and adds
-- overheads, if at all.

module TracePlusParST
       (
         runPar, Par,
         module Control.Par.Class 
       ) where

import qualified Control.Monad.Par.Scheds.Trace as T
import Control.Par.Class
import Control.Par.ST
import qualified Control.Monad.State.Strict as S

type Par d s a = ParST (STUnit s) T.Par a
-- type Par d s1 s0 a = ParST (STUnit s1) (L.Par d s0) a

runPar :: forall a d . (forall s0 . Par d s0 a) -> a
runPar m =
   T.runPar $
   runParST STUnit m
