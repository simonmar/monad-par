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
  (  
    ParFuture(..)
  , ParIVar(..)
  , ParChan(..)
  , ParDist(..)
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

-- | @ParFuture@ captures the class of Par monads which support
--   futures.  This level of functionality subsumes "par/pseq" and is
--   similar to the @Eval@ monad.
class Monad m => ParFuture m future | m -> future where
  -- | Like 'fork', but returns a @IVar@ that can be used to query the
  -- result of the forked computataion.
  --
  -- >  spawn p = do
  -- >    r <- new
  -- >    fork (p >>= put r)
  -- >    return r
  --
  spawn  :: NFData a => m a -> m (future a)
  -- | Like 'spawn', but the result is only head-strict, not fully-strict.
  spawn_ :: m a -> m (future a)
  get    :: future a -> m a

-- | @ParIVar@ builds on futures by adding full anyone-writes, anyone-reads IVars.
--   These are more expressive but may not be supported by all distributed schedulers.
class ParFuture m ivar => ParIVar m ivar | m -> ivar where
--class (Monad m, ParFuture m ivar) => ParIVar m ivar | m -> ivar where
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


-- | @ParChan@ provides communication of a stream of values between
--   computations in a Par monad.  Channels in this case are split
--   into separate send and receive ports.
class ParChan m snd rcv | m -> snd, m -> rcv where
   newChan :: m (snd a, rcv a)
   recv    :: rcv a -> m a
   send    :: snd a -> a -> m ()

--------------------------------------------------------------------------------
-- Distributed operation:
--------------------------------------------------------------------------------

-- There doesn't seem to be a need to have a Future/IVar distinction here.
-- Rather, implementations will or will not make IVars serializable.
-- Likewise, some implementations will make the send ports of channels serializable.
-- (And perhaps they will allow an IVar to be converted to such a send port.)

#define DIST_MONAD_PAR
#ifdef DIST_MONAD_PAR
-- | The @ParDist@ class supplies an interface for spawning
class Monad m => ParDist m var | m -> var where
-- TODO: ADD SERIALIZABILITY CONSTRAINTS!  Change to "Closure":
  -- Serializable a => Closure (m a) -> m (ivar a)
  longSpawn :: NFData a => m a -> m (var a)
   -- Closure (m ()) -> m ()
  longFork  :: m () -> m ()
  -- longFork is only useful with an additional distributed IVar or Chan facility.

-- TODO: Possible move to a different package to factor dependencies.
#endif


----------------------------------------------------------------------------------------------------

-- t1 :: P.Par Int
-- If the ParIVar => ParFuture instance exists the following is sufficient:
t1 :: (ParFuture m v) => m Int
t1 = do 
  x <- spawn (return 3)
  get x

t2 :: (ParIVar m v) => m Int
t2 = do 
  x <- new
  put x "hi"
  return 3


-- TODO: SPECIALIZE generic routines for the default par monad (and possibly ParRNG)?

--  SPECIALISE parMap  :: (NFData b) => (a -> b)     -> [a] -> Par [b] 
-- SPECIALISE parMapM :: (NFData b) => (a -> Par b) -> [a] -> Par [b] 
