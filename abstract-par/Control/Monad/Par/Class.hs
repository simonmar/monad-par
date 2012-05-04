{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, CPP,
     FlexibleInstances, UndecidableInstances
  #-}
-- UndecidableInstances

{-|

    This module establishes a class hierarchy that captures the
    interface(s) for valid Par monads.  In particular, the functionality
    is split into layers: e.g. Futures vs. full IVars vs. Chans (Streams).  
    
    Not all Par monad schedulers must provide all functionality.

    For more documentation of the programming model, see  

      * The "Control.Monad.Par" module in the @monad-par@ package.
      * The wiki/tutorial (<http://www.haskell.org/haskellwiki/Par_Monad:_A_Parallelism_Tutorial>)
      * The original paper (<http://www.cs.indiana.edu/~rrnewton/papers/haskell2011_monad-par.pdf>)
      * Tutorial slides (<http://community.haskell.org/~simonmar/slides/CUFP.pdf>)
      * Other slides: <http://www.cs.ox.ac.uk/ralf.hinze/WG2.8/28/slides/simon.pdf>, 
                      <http://www.cs.indiana.edu/~rrnewton/talks/2011_HaskellSymposium_ParMonad.pdf>

 -}
--  

module Control.Monad.Par.Class 
  (  
  -- * Futures
    ParFuture(..)
  -- * IVars
  , ParIVar(..)
  
    -- RRN: Not releasing this interface until there is a nice implementation of it:
    --  Channels (Streams)
    --  , ParChan(..)

  , NFData()
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
--   typical to simply define `spawn` in terms of `fork`, `new`, and `put`.
class Monad m => ParFuture future m | m -> future where
  -- | Create a potentially-parallel computation, and return a /future/
  -- (or /promise/) that can be used to query the result of the forked
  -- computataion.  
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

  -- | Spawn a pure (rather than monadic) computation.  Fully-strict.
  -- 
  -- >  spawnP = spawn . return
  spawnP :: NFData a =>   a -> m (future a)
  
  -- Default implementations:
  spawn  p = spawn_ (do x <- p; deepseq x (return x))
  spawnP a = spawn (return a)


--------------------------------------------------------------------------------

-- | @ParIVar@ builds on futures by adding full /anyone-writes, anyone-reads/ IVars.
--   These are more expressive but may not be supported by all distributed schedulers.
-- 
-- A minimal implementation consists of `fork`, `put_`, and `new`.
class ParFuture ivar m  => ParIVar ivar m | m -> ivar where
  -- | Forks a computation to happen in parallel.  The forked
  -- computation may exchange values with other computations using
  -- @IVar@s.
  fork :: m () -> m ()
  
  -- | creates a new @IVar@
  new  :: m (ivar a)
  
  -- | put a value into a @IVar@.  Multiple 'put's to the same @IVar@
  -- are not allowed, and result in a runtime error.
  --
  -- 'put' fully evaluates its argument, which therefore must be an
  -- instance of 'NFData'.  The idea is that this forces the work to
  -- happen when we expect it, rather than being passed to the consumer
  -- of the @IVar@ and performed later, which often results in less
  -- parallelism than expected.
  --
  -- Sometimes partial strictness is more appropriate: see 'put_'.
  --
  put  :: NFData a => ivar a -> a -> m ()
  put v a = deepseq a (put_ v a)
  
  -- | like 'put', but only head-strict rather than fully-strict.  
  put_ :: ivar a -> a -> m ()

  -- Extra API routines that have default implementations:
  
  -- | creates a new @IVar@ that contains a value
  newFull :: NFData a => a -> m (ivar a)
  newFull a = deepseq a (newFull_ a)
  
  -- | creates a new @IVar@ that contains a value (head-strict only)
  newFull_ ::  a -> m (ivar a)
  newFull_ a = do v <- new
                  -- This is usually inefficient! 
		  put_ v a
		  return v

--------------------------------------------------------------------------------  

-- class ParYieldable ?? 
  -- TODO: I think we should add yield officially:

  -- Allows other parallel computations to progress.  (should not be
  -- necessary in most cases).
  --  yield  :: m ()


--------------------------------------------------------------------------------

-- | @ParChan@ provides communication via streams of values between
--   computations in a Par monad.  Channels in this case are split
--   into separate send and receive ports.
--
--   The critical thing to know about @Chan@s in @Par@ monads is that
--   while the @recv@ method destructively advances the position of
--   the consumer's \"cursor\" in the stream, this is only observable
--   in the /local/ @Par@ thread.  That is, at @fork@ points it is
--   necessary to give the child computation a separate set of stream
--   cursors so that it observes the same sequences as the parent.
class Monad m => ParChan snd rcv m | m -> snd, m -> rcv where
   -- | Create a new communication channel, with separate send and receive ports.
   newChan :: m (snd a, rcv a)
   -- | Receive a message on a channel in a synchronous, blocking manner.
   recv    :: rcv a -> m a
   -- | Send a message on a channel.  This may or may not block.
   send    :: snd a -> a -> m ()


----------------------------------------------------------------------------------------------------

-- t1 :: P.Par Int
-- If the ParIVar => ParFuture instance exists the following is sufficient:
t1 :: (ParFuture v m) => m Int
t1 = do 
  x <- spawn (return 3)
  get x

t2 :: (ParIVar v m) => m Int
t2 = do 
  x <- new
  put x "hi"
  return 3


-- TODO: SPECIALIZE generic routines for the default par monad (and possibly ParRNG)?

--  SPECIALISE parMap  :: (NFData b) => (a -> b)     -> [a] -> Par [b] 
-- SPECIALISE parMapM :: (NFData b) => (a -> Par b) -> [a] -> Par [b] 
