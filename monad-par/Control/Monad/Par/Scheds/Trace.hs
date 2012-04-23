{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, MultiParamTypeClasses, CPP
	     #-}
{- OPTIONS_GHC -Wall -fno-warn-name-shadowing -fwarn-unused-imports -}

{- | This is the scheduler described in the paper "A Monad for
     Deterministic Parallelism".  It is based on a lazy @Trace@ data
     structure that separates the scheduler from the @Par@ monad
     method implementations.

 -}

module Control.Monad.Par.Scheds.Trace (
    Par, runPar, fork,
    IVar, new, newFull, newFull_, get, put, put_,
    spawn, spawn_, spawnP 
  ) where

import qualified Control.Monad.Par.Class as PC
import Control.Monad.Par.Scheds.TraceInternal
import Control.DeepSeq
import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)

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
