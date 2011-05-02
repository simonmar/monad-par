

-- A class encompassing valid Par monads.

module Control.Monad.ParClass 
  (
    ParClass(..)
  )
where

import Control.DeepSeq
import qualified Control.Monad.Par as P

class Monad m => ParClass m where
--  runPar :: m a -> a
  fork :: m () -> m ()

  new :: m (P.IVar a)

  newFull  :: NFData a => a -> m (P.IVar a)
  newFull_ ::             a -> m (P.IVar a)

  get  :: P.IVar a -> m a
  put  :: NFData a => P.IVar a -> a -> m ()
  put_ ::             P.IVar a -> a -> m ()

--  yield  :: m ()

  pval :: NFData a => a -> m (P.IVar a)
  pval a = spawn (return a)

  spawn :: NFData a => m a -> m (P.IVar a)
  spawn p = do r <- new
	       fork (p >>= put r)
	       return r

  spawn_ :: m a -> m (P.IVar a)
  spawn_ p = do r <- new
		fork (p >>= put_ r)
		return r


instance ParClass P.Par where 
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
