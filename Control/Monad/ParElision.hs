{-# LANGUAGE RankNTypes, ImpredicativeTypes  #-}

-- This is a sequential implementation of the Par monad.

-- It only works for the subset of programs in which a
-- top-to-bottom/left-to-right execution of the program writes all
-- IVars before reading them.


module Control.Monad.ParElision (
    Par, IVar, 
    runPar, 
    fork,
    new, newFull, newFull_,
    get,
    put, put_,
--    both,
    pval,
    spawn, spawn_,

-- runParAsync,  yield,

--    parMap, parMapM, parMapReduceRange, parMapReduceRangeThresh,
--    InclusiveRange(..),
--    parFor

  ) where

import Control.Exception
import Control.DeepSeq
import Control.Monad.ST
-- import Data.STRef
import Data.IORef
import Debug.Trace
import GHC.IO


fork :: Par () -> Par ()
new  :: Par (IVar a)
get  :: IVar a -> Par a
put  :: NFData a => IVar a -> a -> Par ()
put_ :: IVar a -> a -> Par ()
pval :: NFData a => a -> Par (IVar a)
spawn_ :: Par a -> Par (IVar a)
spawn  :: NFData a => Par a -> Par (IVar a)

newFull :: NFData a => a -> Par (IVar a)
newFull_ :: a -> Par (IVar a)
runPar :: Par a -> a


-- type Par a  = forall s. ST s a
-- type IVar a = forall s. STRef s (Maybe a)

-- fork m = do m; return ()
-- new = newSTRef Nothing
-- runPar m = runST m



type Par a  = IO a
type IVar a = IORef (Maybe a)

fork m = do m; return ()

new = newIORef Nothing
newFull x  = rnf x `seq` newIORef (Just x)
newFull_ x = newIORef (Just x)

get r = do x <- readIORef r
	   case x of 
	     Nothing -> error "IVar read before written!"
	     Just y  -> return y 

put  r x = rnf x `seq` writeIORef r (Just x)
put_ r x = writeIORef r (Just x)

pval a = spawn (return a)
spawn  m = do x <- m; evaluate (rnf x); newIORef (Just x)
spawn_ m = do x <- m; newIORef (Just x)

runPar m = 
  trace ("Running par with unsafeIO...")
  unsafePerformIO m 
