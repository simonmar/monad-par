{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, CPP,
     FlexibleInstances, UndecidableInstances
  #-}

-- A class encompassing valid Par monads.

module Control.Monad.Par.Class 
  ( ParFuture(..)
  , ParIVar(..)
  , ParChan(..)
  , ParDist(..)
  )
where

import Control.DeepSeq
import qualified Control.Monad.Par as P

--------------------------------------------------------------------------------
-- The basic layers of the Par monad vary in what data structures they
-- support (Futures, IVars, Streams.)
--------------------------------------------------------------------------------

class Monad m => ParGettable m var | m -> var where
  get :: var a -> m a

class ParGettable m future => ParFuture m future | m -> future where
  spawn  :: NFData a => m a -> m (future a)
  spawn_ :: m a -> m (future a)

class ParGettable m ivar => ParIVar m ivar | m -> ivar where
  fork :: m () -> m ()
  new  :: m (ivar a)
  put_ :: ivar a -> a -> m ()
  put  :: NFData a => ivar a -> a -> m ()
  put v a = deepseq a (put_ v a)

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

class Monad m => ParDist m ivar | m -> ivar where
  longSpawn :: NFData a => m a -> m (ivar a)

class Monad m => ParDistIVar m ivar | m -> ivar where
  longFork  :: m () -> m ()

--------------------------------------------------------------------------------
-- Standard instances:

instance ParGettable P.Par P.IVar where
  get = P.get

instance ParIVar P.Par P.IVar where 
  fork = P.fork 
  new  = P.new
  put  = P.put
  put_ = P.put_
  newFull  = P.newFull
  newFull_ = P.newFull_
--  yield = P.yield
--  get  = P.get

----------------------------------------------------------------------------------------------------

#if 1
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
t1 :: (ParIVar m v, ParFuture m v) => m Int
t1 = do 
  x <- spawn (return 3)
  get x

t2 :: (ParIVar m v) => m Int
t2 = do 
  x <- new
  put x "hi"
  return 3
--  get x




-- TODO: SPECIALIZE generic routines for the default par monad (and possibly ParRNG)?

--  SPECIALISE parMap  :: (NFData b) => (a -> b)     -> [a] -> Par [b] 
-- SPECIALISE parMapM :: (NFData b) => (a -> Par b) -> [a] -> Par [b] 
