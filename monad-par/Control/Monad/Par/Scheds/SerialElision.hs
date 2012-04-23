{-# LANGUAGE RankNTypes, ImpredicativeTypes, MultiParamTypeClasses, 
             CPP
   #-}

-- | This is a sequential implementation of the Par monad.
-- 
--   It only works for the subset of programs in which a
--   top-to-bottom/left-to-right execution of the program writes all
--   IVars before reading them.  It is analogous to the Cilk notion of
--   a \"serial elision\" -- eliding the parallel annotations and
--   running in serial.
--
--   This module can be used for debugging as well as for establishing a
--   serial baseline performance.
--

module Control.Monad.Par.Scheds.SerialElision (
    Par, IVar, runPar, fork,
    new, newFull, newFull_,
    get, put, put_,
    spawn, spawn_, spawnP
  ) where

import qualified Control.Monad.Par.Class as PC
import Control.Exception
import Control.DeepSeq
import Control.Monad.ST
-- import Data.STRef
import Data.IORef
import Debug.Trace
import GHC.IO

--------------------------------------------------------------------------------
-- Central type definitions:
newtype Par a = P (IO a)
newtype IVar a = I (IORef (Maybe a))

unP (P x) = x

-- The newtype's above were necessary for the 'ParIVar'/'ParFuture'
-- instance below and thus we need a Monad instance as well:
instance Monad Par where 
  (P m) >>= f = P (m >>= unP . f)
  return x = P (return x)

--------------------------------------------------------------------------------

fork :: Par () -> Par ()
new  :: Par (IVar a)
get  :: IVar a -> Par a
put  :: NFData a => IVar a -> a -> Par ()
put_ :: IVar a -> a -> Par ()

newFull :: NFData a => a -> Par (IVar a)
newFull_ :: a -> Par (IVar a)
runPar :: Par a -> a

--------------------------------------------------------------------------------

fork (P m) = P$ do m; return ()

new = P$ newIORef Nothing                     >>= return . I
newFull  x = rnf x `seq` P (newIORef (Just x) >>= return . I)
newFull_ x = P$ newIORef (Just x)             >>= return . I

get (I r) = P$
        do x <- readIORef r
	   case x of 
             -- TODO: Could keep track of pedigree for better errors:
	     Nothing -> error "IVar read before written!"
	     Just y  -> return y 

put  (I r) x = rnf x `seq` P (writeIORef r (Just x))
put_ (I r) x = P$ writeIORef r (Just x)

runPar (P m) = 
  trace ("ParElision: Running with unsafeIO...")
  unsafePerformIO m 


----------------------------------------------------------------------------------------------------
-- TEMP: Factor out this boilerplate somehow.
-- <boilerplate>
spawn  :: NFData a => Par a -> Par (IVar a)
spawn_ :: Par a -> Par (IVar a)
spawnP :: NFData a => a -> Par (IVar a)

spawn p  = do r <- new;  fork (p >>= put r);   return r
spawn_ p = do r <- new;  fork (p >>= put_ r);  return r
spawnP a = spawn (return a)

instance PC.ParFuture IVar Par  where
  get    = get
  spawn  = spawn
  spawn_ = spawn_
  spawnP = spawnP

instance PC.ParIVar IVar Par  where
  fork = fork
  new  = new
  put_ = put_
  newFull = newFull
  newFull_ = newFull_
-- </boilerplate>
--------------------------------------------------------------------------------
