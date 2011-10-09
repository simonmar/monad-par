{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, MultiParamTypeClasses
	     #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fwarn-unused-imports #-}

-- | This module provides a monad @Par@, for speeding up pure
-- computations using parallel processors.  It cannot be used for
-- speeding up computations that use IO (for that, see
-- @Control.Concurrent@).  The result of a given @Par@ computation is
-- always the same - ie. it is deterministic, but the computation may
-- be performed more quickly if there are processors available to
-- share the work.
--
-- For example, the following program fragment computes the values of
-- @(f x)@ and @(g x)@ in parallel, and returns a pair of their results:
--
-- >  runPar $ do
-- >      fx <- pval (f x)  -- start evaluating (f x)
-- >      gx <- pval (g x)  -- start evaluating (g x)
-- >      a <- get fx       -- wait for fx
-- >      b <- get gx       -- wait for gx
-- >      return (a,b)      -- return results
--
-- @Par@ can be used for specifying pure parallel computations in
-- which the order of the computation is not known beforehand.
-- The programmer specifies how information flows from one
-- part of the computation to another, but not the order in which
-- computations will be evaluated at runtime.  Information flow is
-- described using "variables" called @IVar@s, which support 'put' and
-- 'get' operations.  For example, suppose you have a problem that
-- can be expressed as a network with four nodes, where @b@ and @c@
-- require the value of @a@, and @d@ requires the value of @b@ and @c@:
--
-- >                       a
-- >                      / \  
-- >                     b   c
-- >                      \ /
-- >                       d
--
-- Then you could express this in the @Par@ monad like this:
--
-- >   runPar $ do
-- >       [a,b,c,d] <- sequence [new,new,new,new]
-- >       fork $ do x <- get a; put b (x+1)
-- >       fork $ do x <- get a; put c (x+2)
-- >       fork $ do x <- get b; y <- get c; put d (x+y)
-- >       fork $ do put a (3 :: Int)
-- >       get d
--
-- The result of the above computation is always 9.  The 'get' operation
-- waits until its input is available; multiple 'put's to the same
-- @IVar@ are not allowed, and result in a runtime error.  Values
-- stored in @IVar@s are usually fully evaluated (although there are
-- ways provided to pass lazy values if necessary).
--
-- In the above example, @b@ and @c@ will be evaluated in parallel.
-- In practice the work involved at each node is too small here to see
-- the benefits of parallelism though: typically each node should
-- involve much more work.  The granularity is completely under your
-- control - too small and the overhead of the @Par@ monad will
-- outweigh any parallelism benefits, whereas if the nodes are too
-- large then there might not be enough parallelism to use all the
-- available processors.
--
-- Unlike @Control.Parallel@, in @Control.Monad.Par@ parallelism is
-- not combined with laziness, so sharing and granulairty are
-- completely under the control of the programmer.  New units of
-- parallel work are only created by @fork@, @par@, and a few other
-- combinators.
--
-- The implementation is based on a work-stealing scheduler that
-- divides the work as evenly as possible between the available
-- processors at runtime.
--

module Control.Monad.Par.Scheds.Trace (
    -- * The @Par@ monad
    Par,
    runPar,
    fork,

    -- * Communication: @IVar@s
    IVar,
    new, newFull, newFull_,
    get,
    put, put_,

    -- * Operations
--    pval, 
    spawn, spawn_

  ) where

import qualified Control.Monad.Par.Class as PC
import Control.Monad.Par.Scheds.TraceInternal
import Control.DeepSeq
import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)

-- {- SPECIALIZE PC.fork :: Par () -> Par () #-}

{-# INLINABLE fork #-}

-- -----------------------------------------------------------------------------

-- | forks a computation to happen in parallel.  The forked
-- computation may exchange values with other computations using
-- @IVar@s.
fork :: Par () -> Par ()
fork p = Par $ \c -> Fork (runCont p (\_ -> Done)) (c ())

-- -----------------------------------------------------------------------------
-- Derived functions

-- | Like 'spawn', but the result is only head-strict, not fully-strict.
spawn_ :: Par a -> Par (IVar a)
spawn_ p = do
  r <- new
  fork (p >>= put_ r)
  return r

-- | Like 'fork', but returns a @IVar@ that can be used to query the
-- result of the forked computataion.
--
-- >  spawn p = do
-- >    r <- new
-- >    fork (p >>= put r)
-- >    return r
--
spawn :: NFData a => Par a -> Par (IVar a)
spawn p = do
  r <- new
  fork (p >>= put r)
  return r

-- --------------------------------------------------------------------------------
-- -- Standard instances:

instance PC.ParGettable Par IVar where
  get = get

instance PC.ParIVar Par IVar where 
  fork = fork 
  new  = new
  put  = put
  put_ = put_
  newFull  = newFull
  newFull_ = newFull_
--  yield = yield
--  get  = get
