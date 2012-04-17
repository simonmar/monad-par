{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.Serial (
    runPar
  , runParIO
  , module Control.Monad.Par.Meta
) where

import Control.Monad.Par.Meta
import qualified Control.Monad.Par.Meta.Resources.SingleThreaded as Single

resource :: Resource
resource = Single.mkResource 

runPar   :: Par a -> a
runParIO :: Par a -> IO a
runPar   = runMetaPar   resource
runParIO = runMetaParIO resource