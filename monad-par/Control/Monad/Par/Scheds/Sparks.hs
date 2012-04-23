{-# LANGUAGE MultiParamTypeClasses #-}

-- | This scheduler uses sparks (par/pseq) directly, but only supplies
--   the @Monad.Par.Class.ParFuture@ interface.

module Control.Monad.Par.Scheds.Sparks
 (
   Par(..), Future(..),
   runPar, 
   get, spawn, spawn_, spawnP
 ) 
where 

import Control.Applicative
import Control.Monad
import Control.DeepSeq
import Control.Parallel
import qualified Control.Monad.Par.Class as PC
-- import Control.Parallel.Strategies (rpar)

{-# INLINE runPar #-}
{-# INLINE spawn #-}
{-# INLINE spawn_ #-}
{-# INLINE spawnP #-}
{-# INLINE get #-}

data Par    a = Done   a
data Future a = Future a

runPar :: Par a -> a
runPar (Done x) = x

spawn_ :: Par a -> Par (Future a)
-- spawn_ a = do a' <- rpar (runPar a); return (Future a')
spawn_ a = let a' = runPar a in a' `par` return (Future a')

spawn :: NFData a => Par a -> Par (Future a)
spawn a = let a' = runPar a in a' `par` return (Future (rnf a' `pseq` a'))

spawnP :: NFData a => a -> Par (Future a)
spawnP a = a `par` return (Future (rnf a `pseq` a))

get :: Future a -> Par a
get (Future a) = a `pseq` return a

--------------------------------------------------------------------------------
-- <boilerplate>

instance Monad Par where
  return x = Done x
  Done x >>= k = k x

instance PC.ParFuture Future Par  where 
  get    = get
  spawn  = spawn
  spawn_ = spawn_
  spawnP = spawnP

instance Functor Par where
   fmap f xs = xs >>= return . f

instance Applicative Par where
   (<*>) = ap
   pure  = return

-- </boilerplate>
--------------------------------------------------------------------------------
