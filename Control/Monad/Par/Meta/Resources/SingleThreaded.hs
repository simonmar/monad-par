{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall #-}

-- | A simple single-threaded resource that is a useful accompaniment
-- for testing non-CPU resources such as GPU or distributed.
module Control.Monad.Par.Meta.Resources.SingleThreaded (
    initAction
  , stealAction
  ) where

import Control.Concurrent ( myThreadId, threadCapability
                          , newQSem, waitQSem )
import Control.Monad

import Text.Printf

import Control.Monad.Par.Meta hiding (dbg, stealAction)

dbg :: Bool
#ifdef DEBUG
dbg = True
#else
dbg = True
#endif

initAction :: InitAction
initAction = IA ia 
  where ia sa _ = do
          qsem <- newQSem 0
          (cap, _) <- threadCapability =<< myThreadId
          printf " [%d] spawning single worker\n" cap
          -- This initAction is called from the "main" thread, we need
          -- to spawn a worker to do the actual work:
          void $ spawnWorkerOnCap' qsem sa cap
          -- We wait on the qsem for the worker to come online and
          -- initialize itself before we return to the main thread and
          -- begin scheduling.
          waitQSem qsem        

-- | In the singlethreaded scenario there are NO other workers from
--   which to steal.
stealAction :: StealAction
stealAction = SA sa 
  where sa _ _ = return Nothing
