{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall #-}

-- | A simple single-threaded 'Resource' that is a useful
-- accompaniment for testing non-CPU resources such as GPU or
-- distributed.
module Control.Monad.Par.Meta.Resources.SingleThreaded ( 
    -- * Resource creation
    mkResource
    -- * Implementation
  , defaultStartup
  , defaultWorkSearch
) where

import Control.Concurrent
import Control.Monad

import Text.Printf

import Control.Monad.Par.Meta

dbg :: Bool
#ifdef DEBUG
dbg = True
#else
dbg = True
#endif

-- | Create a single-threaded resource.
mkResource :: Resource
mkResource = Resource defaultStartup defaultWorkSearch

-- | Spawn a single Meta-Par worker.
defaultStartup :: Startup
defaultStartup = St st 
  where st ws _ = do
#if __GLASGOW_HASKELL__ >= 702
          (cap, _) <- threadCapability =<< myThreadId
#else
          -- Older GHCs: run on CPU 0 if we can't ask for caller's capability
          let cap = 0
#endif
          when dbg $ printf " [%d] spawning single worker\n" cap
          -- This startup is called from the "main" thread, we need
          -- to spawn a worker to do the actual work:
          spawnWorkerOnCPU ws cap >> return ()

-- | A single-threaded resource by itself is not aware of any other
-- sources of work, so its 'WorkSearch' always returns 'Nothing'.
defaultWorkSearch :: WorkSearch
defaultWorkSearch = WS ws 
  where ws _ _ = return Nothing
