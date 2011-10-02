{-# LANGUAGE RankNTypes, ImpredicativeTypes, MultiParamTypeClasses
   #-}
--    TypeSynonymInstances

-- | This is a sequential implementation of the Par monad.
-- 
--   It only works for the subset of programs in which a
--   top-to-bottom/left-to-right execution of the program writes all
--   IVars before reading them.  It is analogous to the Cilk notion of
--   a "serial elision" -- eliding the parallel annotations and
--   running in serial.
--
--   This module can be used for debugging as well as for establishing a
--   serial baseline performance.
--

module Control.Monad.ParElision (
    Par, IVar, runPar, fork,
    new, newFull, newFull_,
    get, put, put_,
    pval, spawn, spawn_,
  ) where

import qualified Control.Monad.ParClass as PC
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
-- newtype Par a = Par (IO a)
newtype IVar a = I (IORef (Maybe a))

unP (P x) = x

-- The newtype's above were necessary for the ParClass instance below
-- and thus we need a Monad instance as well:
instance Monad Par where 
  (P m) >>= f = P (m >>= unP . f)
  return x = P (return x)

instance PC.ParClass Par IVar where 
  fork = fork 
  new  = new
  get  = get
  put  = put
  put_ = put_
  newFull  = newFull
  newFull_ = newFull_

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


--------------------------------------------------------------------------------
-- TEMP: Not strictly needed:

pval :: NFData a => a -> Par (IVar a)
spawn_ :: Par a -> Par (IVar a)
spawn  :: NFData a => Par a -> Par (IVar a)

pval a = spawn (P$ return a)
spawn  (P m) = P$ do x <- m; 
		     evaluate (rnf x); 
		     ref <- newIORef (Just x); 
		     return (I ref)
spawn_ (P m) = P$ do x <- m; 
		     ref <- newIORef (Just x); 
		     return (I ref)
