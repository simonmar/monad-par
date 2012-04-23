{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.SMP (
    runPar
  , runParIO
  , module Control.Monad.Par.Class
  , module Control.Monad.Par.Meta
) where

import Control.Monad.Par.Class
import Control.Monad.Par.Meta
import qualified Control.Monad.Par.Meta.Resources.SMP as SMP

tries :: Int
tries = 20

resource :: Resource
resource = SMP.mkResource tries

runPar   :: Par a -> a
runParIO :: Par a -> IO a
runPar   = runMetaPar   resource
runParIO = runMetaParIO resource