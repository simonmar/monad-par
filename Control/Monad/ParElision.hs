{-# LANGUAGE RankNTypes, ImpredicativeTypes, TypeSynonymInstances  #-}

-- This is a sequential implementation of the Par monad.

-- It only works for the subset of programs in which a
-- top-to-bottom/left-to-right execution of the program writes all
-- IVars before reading them.

-- This module can be used for debugging as well as for establishing a
-- serial baseline performance.


module Control.Monad.ParElision (
    Par, IVar, runPar, fork,
    new, newFull, newFull_,
    get, put, put_,
    pval, spawn, spawn_,

-- runParAsync,  yield,

--    parMap, parMapM, parMapReduceRange, parMapReduceRangeThresh,
--    InclusiveRange(..),
--    parFor

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
type Par a = IO a
-- newtype Par a = Par (IO a)
type IVar a = IORef (Maybe a)

-- TODO:
-- instance PC.ParClass Par where 
--   fork = fork 
--   new  = new
--   get  = get
--   put  = put
--   put_ = put_
--   newFull  = newFull
--   newFull_ = newFull_

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

fork m = do m; return ()

new = newIORef Nothing
newFull x  = rnf x `seq` newIORef (Just x)
newFull_ x = newIORef (Just x)

get r = do x <- readIORef r
	   case x of 
             -- TODO: Could keep track of pedigree for better errors:
	     Nothing -> error "IVar read before written!"
	     Just y  -> return y 

put  r x = rnf x `seq` writeIORef r (Just x)
put_ r x = writeIORef r (Just x)

runPar m = 
  trace ("ParElision: Running with unsafeIO...")
  unsafePerformIO m 


--------------------------------------------------------------------------------
-- TEMP: Not strictly needed:

pval :: NFData a => a -> Par (IVar a)
spawn_ :: Par a -> Par (IVar a)
spawn  :: NFData a => Par a -> Par (IVar a)

pval a = spawn (return a)
spawn  m = do x <- m; evaluate (rnf x); newIORef (Just x)
spawn_ m = do x <- m; newIORef (Just x)
