{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP
	     #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fwarn-unused-imports #-}

{- 

  This alternate implementation of Par uses the existing thread/MVar
  capabilities of GHC.  It therefore represents an attempt to do
  monad-par style programming before monad-par itself was implemented.

 -}


module Control.Monad.Par_Strawman (
    Par, IVar,
    runPar, runParAsync,
    fork,
    new, newFull, newFull_,
    get,
    put, put_,
    pval,
    spawn, spawn_,
    parMap, parMapM, parMapReduceRange,

  ) where

import Control.Concurrent

import Data.Traversable
import Control.Monad hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent
import GHC.Conc hiding ()
import Control.DeepSeq
import Control.Applicative
-- import Text.Printf

-- For testing only:
import Test.HUnit 
import Control.Exception


-- ---------------------------------------------------------------------------


type Par a = IO a

runPar = unsafePerformIO
runParAsync = runPar

-- instance Applicative Par where
--    (<*>) = ap
--    pure  = return

type IVar a = MVar a

fork :: Par () -> Par ()
fork p = do forkIO p; return ()

new :: Par (MVar a)
new = newEmptyMVar

newFull :: NFData a => a -> Par (IVar a)
newFull x = deepseq x (newMVar x)

newFull_ :: a -> Par (IVar a)
newFull_ !x = newMVar x

get :: IVar a -> Par a
get = readMVar

put_ :: IVar a -> a -> Par ()
put_ v !a = putMVar v a

put :: NFData a => IVar a -> a -> Par ()
put v a = deepseq a (putMVar v a)

--  < BEGIN DUPLICATED CODE >

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
spawn :: NFData a => Par a -> Par (IVar a)
spawn p = do
  r <- new
  fork (p >>= put r)
  return r

-- | equivalent to @spawn . return@
pval :: NFData a => a -> Par (IVar a)
pval a = spawn (return a)

-- -----------------------------------------------------------------------------
-- Parallel maps over Traversable data structures

parMap :: (Traversable t, NFData b) => (a -> b) -> t a -> Par (t b)
parMap f xs = mapM (pval . f) xs >>= mapM get

parMapM :: (Traversable t, NFData b) => (a -> Par b) -> t a -> Par (t b)
parMapM f xs = mapM (spawn . f) xs >>= mapM get

{-# SPECIALISE parMap  :: (NFData b) => (a -> b)     -> [a] -> Par [b] #-}
{-# SPECIALISE parMapM :: (NFData b) => (a -> Par b) -> [a] -> Par [b] #-}


-- | parMapReduceRange is similar to the "parallel for" construct
--   found in many parallel programming models.  
-- 
parMapReduceRange :: NFData a => Int -> Int -> Int -> (Int -> Par a) -> (a -> a -> Par a) -> a -> Par a
parMapReduceRange threshold min max fn binop init = loop min max 
 where 
  loop min max 
    | max - min <= threshold = 
	let mapred a b = do x <- fn b; 
			    result <- a `binop` x
			    return result 
	in foldM mapred init [min..max]

    | otherwise  = do
	let mid = min + ((max - min) `quot` 2)
	rght <- spawn $ loop (mid+1) max 
	l  <- loop  min    mid 
	r  <- get rght
	lr <- l `binop` r
	return lr

-- TODO: A version that works for any splittable input domain.  In this case
-- the "threshold" is a predicate on inputs.
-- parMapReduceRangeGeneric :: (inp -> Bool) -> (inp -> Maybe (inp,inp)) -> inp -> 

