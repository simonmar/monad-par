module Control.Monad.Par.Meta.SharedMemoryOnly (
    runPar
  , runParIO
  , module Control.Monad.Par.Meta
) where

import Control.Monad.Par.Meta
import qualified Control.Monad.Par.Meta.Resources.SharedMemory as SharedMemory

import GHC.Conc

tries = 20
caps  = numCapabilities

ia = SharedMemory.initAction tries
sa = SharedMemory.stealAction caps tries

runPar   = runMetaPar   ia sa
runParIO = runMetaParIO ia sa