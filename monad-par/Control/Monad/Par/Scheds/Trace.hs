{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, MultiParamTypeClasses, CPP #-}
{- OPTIONS_GHC -Wall -fno-warn-name-shadowing -fwarn-unused-imports -}

{-# LANGUAGE TypeFamilies #-}

{- | This is the scheduler described in the paper "A Monad for
     Deterministic Parallelism".  It is based on a lazy @Trace@ data
     structure that separates the scheduler from the @Par@ monad
     method implementations.

 -}

module Control.Monad.Par.Scheds.Trace (
    Par, runPar, runParIO, fork,
    IVar, new, newFull, newFull_, get, put, put_,
    spawn, spawn_, spawnP
  ) where

import qualified Control.Monad.Par.Class as PC
import Control.Monad.Par.Scheds.TraceInternal
import Control.DeepSeq
import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)

#ifdef NEW_GENERIC
import qualified       Control.Par.Class as PN
import qualified       Control.Par.Class.Unsafe as PU
#endif

-- -----------------------------------------------------------------------------

-- Not in 6.12: {- INLINABLE fork -}
{-# INLINE fork #-}
fork :: Par () -> Par ()
fork p = Par $ \c -> Fork (runCont p (\_ -> Done)) (c ())

-- --------------------------------------------------------------------------------
-- -- Standard instances:

-- <boilerplate>
spawn p  = do r <- new;  fork (p >>= put r);   return r
spawn_ p = do r <- new;  fork (p >>= put_ r);  return r
-- </boilerplate>>

spawnP :: NFData a => a -> Par (IVar a)
spawnP a = spawn (return a)

instance PC.ParFuture IVar Par  where
  get    = get
  spawn  = spawn
  spawn_ = spawn_
  spawnP = spawnP

instance PC.ParIVar IVar Par  where
  fork = fork
  new  = new
  put  = put
  put_ = put_
  newFull  = newFull
  newFull_ = newFull_
--  yield = yield

#ifdef NEW_GENERIC
instance PU.ParMonad Par where
  fork = fork  
  internalLiftIO io = Par (LiftIO io)

instance PU.ParThreadSafe Par where
  unsafeParIO io = Par (LiftIO io)  
    
instance PN.ParFuture Par where
  type Future Par = IVar
  type FutContents Par a = ()
  get    = get
  spawn  = spawn
  spawn_ = spawn_
  spawnP = spawnP
  
instance PN.ParIVar Par  where
  new  = new
  put_ = put_
  newFull = newFull
  newFull_ = newFull_
#endif

