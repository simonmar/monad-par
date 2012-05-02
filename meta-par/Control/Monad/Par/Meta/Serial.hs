{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#if __GLASGOW_HASKELL__ >= 702
{-# Language Trustworthy #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.Serial (
    -- * Meta-Par monad for single-threaded execution
    Par
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
import qualified Control.Monad.Par.Meta.Resources.SingleThreaded as Single

-- | The Meta-Par monad specialized for single-threaded execution.
newtype Par a = Par { unPar :: Meta.Par a }
  deriving (Functor, Applicative, Monad,
            ParFuture Meta.IVar, ParIVar Meta.IVar)

resource :: Meta.Resource
resource = Single.mkResource 

runPar   :: Par a -> a
runParIO :: Par a -> IO a
runPar   = Meta.runMetaPar   resource . unPar
runParIO = Meta.runMetaParIO resource . unPar