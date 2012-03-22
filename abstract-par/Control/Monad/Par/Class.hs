{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, CPP,
     FlexibleInstances, UndecidableInstances
  #-}
-- UndecidableInstances

{-|

    This module establishes a class hierarchy that captures the
    interface(s) for valid Par monads.  In particular, the functionality
    is split into layers: e.g. Futures vs. full IVars vs. Chans (Streams).  
    
    Not all Par monad schedulers must provide all functionality.

    For documentation of individual Par functions, please see "Control.Monad.Par".

 -}

module Control.Monad.Par.Class 
  (  
  -- * Futures
    ParFuture(..)
  -- * IVars
  , ParIVar(..)
  -- * Channels (Streams)
  , ParChan(..)
-- define DIST_MONAD_PAR
#ifdef DIST_MONAD_PAR
  -- * Distributed Par monads
  , ParDist(..)
#endif
  )
where

import Control.DeepSeq

--------------------------------------------------------------------------------

-- | @ParFuture@ captures the class of Par monads which support
--   futures.  This level of functionality subsumes @par@/@pseq@ and is
--   similar to the "Control.Parallel.Strategies.Eval" monad.
-- 
--   A minimal implementation consists of `spawn_` and `get`.
--   However, for monads that are also a member of `ParIVar` it is
--   typical to simple define `spawn` in terms of `fork`, `new`, and `put`.
class Monad m => ParFuture m future | m -> future where
  spawn  :: NFData a => m a -> m (future a)
  spawnP :: NFData a =>   a -> m (future a)
  spawn_ :: m a -> m (future a)
  get    :: future a -> m a

  -- Default implementations:
  spawn  p = spawn_ (do x <- p; deepseq x (return x))
  spawnP a = spawn (return a)


--------------------------------------------------------------------------------

-- | @ParIVar@ builds on futures by adding full /anyone-writes, anyone-reads/ IVars.
--   These are more expressive but may not be supported by all distributed schedulers.
class ParFuture m ivar => ParIVar m ivar | m -> ivar where
  fork :: m () -> m ()
  new  :: m (ivar a)
  put  :: NFData a => ivar a -> a -> m ()
  put v a = deepseq a (put_ v a)
  put_ :: ivar a -> a -> m ()

  -- Extra API routines that have default implementations:

  newFull :: NFData a => a -> m (ivar a)
  newFull a = deepseq a (newFull_ a)

  newFull_ ::  a -> m (ivar a)
  newFull_ a = do v <- new
                  -- This is usually inefficient! 
		  put_ v a
		  return v

  -- TODO: I think we should add yield officially:

  -- | Allows other parallel computations to progress.  (should not be
  -- necessary in most cases).
  --  yield  :: m ()


-- | @ParChan@ provides communication via streams of values between
--   computations in a Par monad.  Channels in this case are split
--   into separate send and receive ports.
--
--   The critical thing to know about @Chan@s in @Par@ monads is that
--   while the @recv@ method destructively advances the position of
--   the consumers \"cursor\" in the stream, this is only observable in
--   the local @Par@ thread.  That is, at @fork@ points it is
--   necessary to give the child a separate set of stream cursors so
--   that it observes the same sequences as the parent.
class Monad m => ParChan m snd rcv | m -> snd, m -> rcv where
   newChan :: m (snd a, rcv a)
   recv    :: rcv a -> m a
   send    :: snd a -> a -> m ()


--------------------------------------------------------------------------------
-- Foreign Computations
--------------------------------------------------------------------------------

-- | These are computations outside the normal 

class Monad m => ParDist m var | m -> var where

--------------------------------------------------------------------------------
-- Distributed operation:
--------------------------------------------------------------------------------

-- There doesn't seem to be a need to have a Future/IVar distinction here.
-- Rather, implementations will or will not make IVars serializable.
-- Likewise, some implementations will make the send ports of channels serializable.
-- (And perhaps they will allow an IVar to be converted to such a send port.)


#ifdef DIST_MONAD_PAR
-- | The @ParDist@ class supplies an interface for spawning parallel
--   computations that may potentially run on other machines.
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
