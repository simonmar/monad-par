module Control.Monad.Par.Meta.SharedMemoryAccelerate (
    runPar
  , runParIO
  , Accelerate.spawnAcc
  , module Control.Monad.Par.Meta
) where

import Control.Monad.Par.Meta
import qualified Control.Monad.Par.Meta.Resources.Accelerate as Accelerate
import qualified Control.Monad.Par.Meta.Resources.SharedMemory as SharedMemory

import GHC.Conc

tries = 20
caps  = numCapabilities

ia = SharedMemory.initAction tries >> Accelerate.initAction
sa = SharedMemory.stealAction caps tries >> Accelerate.stealAction

runPar   = runMetaPar   ia sa
runParIO = runMetaParIO ia sa