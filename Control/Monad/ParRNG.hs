{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification,
             PackageImports, ScopedTypeVariables, MultiParamTypeClasses
	     #-}

-- TODO: If we lift the Par family of monads into a class then ParRNG
-- would be an instance.

-- Instead, this is an alternative (and separate) monad.

module Control.Monad.ParRNG
  ( 
     rand, 
     Par, runParRNG, fork,
     IVar, new, newFull, newFull_, get, put, put_,

     pval, spawn, spawn_, 
-- -- parMap, parMapM, parMapReduceRangeThresh,
-- --    parMapReduceRange, InclusiveRange(..), parFor
  )
where

import qualified "mtl" Control.Monad.State.Strict as S
-- import qualified Control.Monad.Trans.State.Strict as ST

import qualified Control.Monad.Par as P
import qualified Control.Monad.ParClass as PC

-- These are reexported without modification:
import Control.Monad.Par (IVar)
import Control.DeepSeq 

import Data.Time.Clock
import System.Random
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- Type Definitions

-- The idea here is to simple route the state of the RNG through the
-- control flow in the Par monad, splitting the RNG where fork is
-- called.
-- data Par a = PRNG (S.StateT StdGen P.Par a)

-- Generic version:
data Par a = PRNG (forall g . RandomGen g => S.StateT g P.Par a)

unPRNG (PRNG x) = x

--------------------------------------------------------------------------------
-- Instances

instance Monad Par where 
  (PRNG sm) >>= f =  PRNG (sm >>= unPRNG . f)
  return x = PRNG (return x)

instance PC.ParClass Par P.IVar where 
  fork = fork 
  new  = new
  get  = get
  put  = put
  put_ = put_
  newFull  = newFull
  newFull_ = newFull_
--  yield = yield


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

-- Produce a randomized value and advance the RNG on the current
-- thread of the parallel computation.  This function is the only
-- difference between this file's API and Par.hs.
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


--------------------------------------------------------------------------------
-- TEMP: These should be subsumed by the default definitions in the ParClass monad:
pval :: NFData a => a -> Par (IVar a)
pval a = spawn (return a)

spawn :: NFData a => Par a -> Par (IVar a)
spawn p = do r <- new
	     fork (p >>= put r)
	     return r

spawn_ :: Par a -> Par (IVar a)
spawn_ p = do r <- new
	      fork (p >>= put_ r)
	      return r
