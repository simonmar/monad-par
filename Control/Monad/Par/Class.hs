{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, CPP,
     FlexibleInstances, UndecidableInstances
  #-}

{-|

    This module establishes a class hierarchy that captures the
    interface for valid Par monads.  In particular, the functionality
    is split into layers: for example Futures vs. full IVars.  

    Not all Par monad schedulers must provide all functionality.

 -}

module Control.Monad.Par.Class 
  ( MonadPar(..)
--  , ParFuture(..)
--  , ParIVar(..)
--  , ParChan(..)
  , ParDist(..)
--  , ParGettable(..)
  )
where

import Control.DeepSeq
-- import qualified Control.Monad.Par.Scheds.Direct as P

--------------------------------------------------------------------------------
-- The basic layers of the Par monad vary in what data structures they
-- support (Futures, IVars, Streams.) Eventually, we would like the
-- class structure to reflect this. For now, though, a monolithic
-- class allows simpler experimentation
--------------------------------------------------------------------------------

class Monad m => MonadPar m var | m -> var where
  get :: var a -> m a
  spawn  :: NFData a => m a -> m (var a)
  spawn p = do r <- new
	       fork (p >>= put r)
	       return r
  spawn_ :: m a -> m (var a)
  spawn_ p = do r <- new
		fork (p >>= put_ r)
		return r
  fork :: m () -> m ()
  new  :: m (var a)
  put_ :: var a -> a -> m ()
  put  :: NFData a => var a -> a -> m ()
  put v a = deepseq a (put_ v a)

  -- TODO: I think we should add yield officially:
--  yield  :: m ()

  -- Extra API routines that have default implementations:
  newFull_ ::  a -> m (var a)
  -- The following is usually inefficient! 
  newFull_ a = do v <- new
		  put_ v a
		  return v

  newFull :: NFData a => a -> m (var a)
  newFull a = deepseq a (newFull_ a)

-- class ParStream m strm | m -> strm where
--   pop  :: strm a -> m a 
--   push :: 

-- Channels with split send and receive ports:
class ParChan m snd rcv | m -> snd, m -> rcv where
   newChan :: m (snd a, rcv a)
--   receive :: rcv a -> m a
   recv    :: rcv a -> m a
   send    :: snd a -> a -> m ()

--------------------------------------------------------------------------------
-- Distributed operation:
--------------------------------------------------------------------------------

-- There doesn't seem to be a need to have a Future/IVar distinction here.
-- Rather, implementations will or will not make IVars serializable.
-- Likewise, some implementations will make the send ports of channels serializable.
-- (And perhaps they will allow an IVar to be converted to such a send port.)

class Monad m => ParDist m ivar | m -> ivar where
  longSpawn :: NFData a => m a -> m (ivar a)
  longFork  :: m () -> m ()


----------------------------------------------------------------------------------------------------

#if 0
instance ParIVar m var => ParFuture m var where 
  spawn p = do r <- new
	       fork (p >>= put r)
	       return r
  spawn_ p = do r <- new
		fork (p >>= put_ r)
		return r
#endif

----------------------------------------------------------------------------------------------------

-- t1 :: P.Par Int
-- If the ParIVar => ParFuture instance exists the following is sufficient:
-- t1 :: ParIVar m v => m Int
t1 :: (MonadPar m v) => m Int
t1 = do 
  x <- spawn (return 3)
  get x

t2 :: (MonadPar m v) => m Int
t2 = do 
  x <- new
  put x "hi"
  return 3


-- TODO: SPECIALIZE generic routines for the default par monad (and possibly ParRNG)?

--  SPECIALISE parMap  :: (NFData b) => (a -> b)     -> [a] -> Par [b] 
-- SPECIALISE parMapM :: (NFData b) => (a -> Par b) -> [a] -> Par [b] 
