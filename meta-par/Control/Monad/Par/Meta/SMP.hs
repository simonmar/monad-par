{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#if __GLASGOW_HASKELL__ >= 702
{-# Language Trustworthy #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.SMP (
    -- * Meta-Par monad for SMP parallelism
    Par
  , Meta.IVar
    -- * Operations
  , ParFuture(..)
  , ParIVar(..)
    -- * Entrypoints
  , runPar
  , runParIO
) where

import Control.Applicative (Applicative)
import Control.Monad.Par.Class
import qualified Control.Monad.Par.Meta as Meta
import qualified Control.Monad.Par.Meta.Resources.SMP as SMP

-- | The Meta-Par monad specialized for SMP parallelism.
newtype Par a = Par { unPar :: Meta.Par a }
  deriving (Functor, Applicative, Monad,
            ParFuture Meta.IVar, ParIVar Meta.IVar)

-- | Number of times to attempt stealing from other SMP workers before
-- continuing in Meta-Par worker loop.
tries :: Int
tries = 20

resource :: Meta.Resource
resource = SMP.mkResource tries

runPar   :: Par a -> a
runParIO :: Par a -> IO a
runPar   = Meta.runMetaPar   resource . unPar
runParIO = Meta.runMetaParIO resource . unPar