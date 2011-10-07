{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- A class encompassing valid Par monads.

module Control.Monad.Par.Class 
  (
    ParClass(..)
  )
where

import Control.DeepSeq
import qualified Control.Monad.Par as P

class Monad m => ParClass m ivar | m -> ivar where
  fork :: m () -> m ()
  new  :: m (ivar a)
  get  :: ivar a -> m a
  put_ :: ivar a -> a -> m ()

  -- TODO: I think we should add yield officially:
--  yield  :: m ()

  -- Extra API routines that have default implementations:

  newFull_ ::  a -> m (ivar a)
  -- The following is usually inefficient! 
  newFull_ a = do v <- new
		  put_ v a
		  return v

  newFull :: NFData a => a -> m (ivar a)
  newFull a = deepseq a (newFull_ a)

  put :: NFData a => ivar a -> a -> m ()
  put v a = deepseq a (put_ v a)

  pval :: NFData a => a -> m (ivar a)
  pval a = spawn (return a)

  spawn :: NFData a => m a -> m (ivar a)
  spawn p = do r <- new
	       fork (p >>= put r)
	       return r

  spawn_ :: m a -> m (ivar a)
  spawn_ p = do r <- new
		fork (p >>= put_ r)
		return r


instance ParClass P.Par P.IVar where 
--  runPar = P.runPar
  fork = P.fork 
  new  = P.new
  get  = P.get
  put  = P.put
  put_ = P.put_
  newFull  = P.newFull
  newFull_ = P.newFull_
--  yield = P.yield


-- TODO: SPECIALIZE generic routines for the default par monad (and possibly ParRNG)?

--  SPECIALISE parMap  :: (NFData b) => (a -> b)     -> [a] -> Par [b] 
-- SPECIALISE parMapM :: (NFData b) => (a -> Par b) -> [a] -> Par [b] 
