{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP,
             PackageImports, ScopedTypeVariables, MultiParamTypeClasses
	     #-}

-- | This module provides adds deterministic parallel random number
--   generation as an additional capability for a 'PC.ParIVar' monad.

module Control.Monad.Par.RNG
  ( 
     rand, 
     Par, runParRNG, fork,
     IVar, new, newFull, newFull_, get, put, put_,
     spawn, spawn_, spawnP
  )
where

-- import qualified "mtl" Control.Monad.State.Strict as S
import qualified "transformers" Control.Monad.Trans.State.Strict as S
import qualified "transformers" Control.Monad.Trans.Class as S

import qualified Control.Monad.Par as P
import qualified Control.Monad.Par.Class as PC

-- These are reexported without modification:
import Control.Monad.Par (IVar)
import Control.DeepSeq 

-- import Data.Time.Clock
import System.Random
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- Type Definitions

-- TODO: Generalize over Par monads.

-- The idea here is to simple route the state of the RNG through the
-- control flow in the Par monad, splitting the RNG where fork is
-- called.
data Par a = PRNG (forall g . RandomGen g => S.StateT g P.Par a)

unPRNG (PRNG x) = x

--------------------------------------------------------------------------------
-- Instances

instance Monad Par where 
  (PRNG sm) >>= f =  PRNG (sm >>= unPRNG . f)
  return x = PRNG (return x)

--------------------------------------------------------------------------------
-- Par API + rand

--runParRNG :: StdGen -> Par a -> a
runParRNG :: RandomGen g => g -> Par a -> a
runParRNG initgen (PRNG sm) = P.runPar $ do
   (a,_) <- S.runStateT sm initgen
   return a

fork :: Par() -> Par()
fork (PRNG sm) = PRNG$
  do g <- S.get 
     let (g1,g2) = split g
         child = do (a,_) <- S.runStateT sm g2; return a
     S.lift$ P.fork child
     S.put g1
     return ()

-- | `rand` is the only new method added by ParRNG over 'PC.ParIVar'.
-- 
--    It produce a randomized value and advances the RNG on the current
--    thread of the parallel computation.  
rand :: Random a => Par a
rand = PRNG$ do
  g <- S.get
  let (a,g') = random g
  S.put g'
  return a


--------------------------------------------------------------------------------
-- These bindings are essentially unchanged.  Nevertheless, some
-- boilerplate (lifting) is needed:

new      :: Par (IVar a)
newFull_ :: a -> Par (IVar a)
newFull  :: NFData a => a -> Par (IVar a)
get      :: IVar a -> Par a
put_     :: IVar a -> a -> Par ()
put      :: NFData a => IVar a -> a -> Par ()


new        = PRNG$ S.lift$ P.new
newFull  x = PRNG$ S.lift$ P.newFull x
newFull_ x = PRNG$ S.lift$ P.newFull_ x
get v      = PRNG$ S.lift$ P.get v
put  v x   = PRNG$ S.lift$ P.put  v x
put_ v x   = PRNG$ S.lift$ P.put_ v x


----------------------------------------------------------------------------------------------------
-- TEMP: Factor out this boilerplate somehow.
-- <boilerplate>
spawn  :: NFData a => Par a -> Par (IVar a)
spawn_ :: Par a -> Par (IVar a)
spawnP :: NFData a => a -> Par (IVar a)

spawn p  = do r <- new;  fork (p >>= put r);   return r
spawn_ p = do r <- new;  fork (p >>= put_ r);  return r
spawnP a = spawn (return a)

instance PC.ParFuture Par IVar where
  get    = get
  spawn  = spawn
  spawn_ = spawn_
  spawnP = spawnP

instance PC.ParIVar Par IVar where
  fork = fork
  new  = new
  put_ = put_
  newFull = newFull
  newFull_ = newFull_
-- </boilerplate>
--------------------------------------------------------------------------------
