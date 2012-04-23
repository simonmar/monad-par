{-# LANGUAGE FlexibleInstances, UndecidableInstances  #-}

-- | This module defines another Par-related class to capture the
--   random number generation capability.  
-- 
--   The `rand` operation provides deterministic parallel random
--   number generation from within a Par monad.
-- 
--   Most likely one will simply use the `ParRand` the instance
--   provided in this file, which is based on a state transformer
--   carrying the random generator.


module Control.Monad.Par.RNG 
 (
  ParRand(..), runParRand, ParRandStd
 ) where 

import System.Random
import Control.Exception

import Control.Monad.Par.Class
import Control.Monad.Par.State
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict as S 

-- | A `ParRand` monad is a Par monad with support for random number generation..
class ParRand p where 
  rand :: Random a => p a
  -- This can be more efficient:
  randInt :: p Int
  randInt = rand 

-- | Trivial instance.
instance RandomGen g => SplittableState g where
  splitState = split

-- | The most straightforward way to get a `ParRand` monad: carry a
--   RNG in a state transformer.
instance (ParFuture fut p, RandomGen g) => ParRand (StateT g p) where 
  rand    = do 
               g <- S.get
	       let (x,g') = random g 
	       S.put g'
	       return x
  randInt = do 
               g <- S.get
               let (x,g') = next g
	       S.put g'
	       return x

-- An alternative is for these operators to be standalone without a class:
-- rand    :: (ParFuture p fut, RandomGen g, Random a) => StateT g p a
-- randInt :: (ParFuture p fut, RandomGen g)           => StateT g p Int

-- runParRand :: ParRand p => (p a -> a) -> p a -> IO a
runParRand :: ParFuture fut p => (p a -> a) -> StateT StdGen p a -> IO a
runParRand runPar m = 
  do g <- newStdGen
     evaluate (runPar (evalStateT m g))


-- | A convenience type for the most standard
type ParRandStd par a = StateT StdGen par a 
