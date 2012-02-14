module Control.Monad.Par.Meta.Dist (
--    runPar
--  , runParIO
    runParDist
  , runParSlave
  , RemoteRsrc.longSpawn
  , module Control.Monad.Par.Meta
) where

import Control.Monad.Par.Meta
import qualified Control.Monad.Par.Meta.Resources.Remote as RemoteRsrc
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment (getEnvironment)
import Data.List (lookup)
import Control.Monad (liftM)
import Control.Monad.Par.Meta.HotVar.IORef

import GHC.Conc

-- tries = 20
-- caps  = numCapabilities

ia scheds = 
     do env <- getEnvironment        
	ml <- case lookup "MACHINE_LIST" env of 
	       Just str -> return (words str)
	       Nothing -> 
		 case lookup "MACHINE_LIST_FILE" env of 
		   Just fl -> liftM words $ readFile fl
  	  	   Nothing -> error$ "Remote resource: Expected to find machine list in "++
			             "env var MACHINE_LIST or file name in MACHINE_LIST_FILE."
        RemoteRsrc.initAction (RemoteRsrc.Master$ map BS.pack ml) scheds
sa = RemoteRsrc.stealAction 

--runPar   = runMetaPar   ia sa
runParDist = runMetaParIO ia sa

runParSlave = do
  RemoteRsrc.taggedMsg "runParSlave invoked."
  schedmap <- readHotVar globalScheds
  RemoteRsrc.initAction RemoteRsrc.Slave globalScheds
  tid      <- myThreadId
  (cap, _) <- threadCapability tid
--  mysched  <- getSchedForCap cap
  mysched  <- makeOrGetSched RemoteRsrc.stealAction cap

  RemoteRsrc.taggedMsg "Slave running simple stealAction loop until shutdown..."
  let schedloop = do RemoteRsrc.stealAction mysched globalScheds
		     schedloop
  schedloop

-- [RemoteCallMetaData]
