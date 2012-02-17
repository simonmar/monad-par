module Control.Monad.Par.Meta.Dist (
--    runPar
--  , runParIO
    runParDist
  , runParSlave
  , shutdownDist
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

import System.Random (randomIO)
import Remote2.Reg (registerCalls)

import GHC.Conc

-- tries = 20
-- caps  = numCapabilities

ia metadata sa scheds = 
     do env <- getEnvironment        
	ml <- case lookup "MACHINE_LIST" env of 
	       Just str -> return (words str)
	       Nothing -> 
		 case lookup "MACHINE_LIST_FILE" env of 
		   Just fl -> liftM words $ readFile fl
  	  	   Nothing -> error$ "Remote resource: Expected to find machine list in "++
			             "env var MACHINE_LIST or file name in MACHINE_LIST_FILE."
        RemoteRsrc.initAction metadata (RemoteRsrc.Master$ map BS.pack ml) sa scheds
sa :: StealAction
sa = RemoteRsrc.stealAction 

--runPar   = runMetaPar   ia sa
runParDist metadata = runMetaParIO (ia metadata) sa

-- When global initialization has already happened:
-- runParDistNested = runMetaParIO (ia Nothing) sa

runParSlave metadata = do
  RemoteRsrc.taggedMsg "runParSlave invoked."
--  registerCalls metadata
--  RemoteRsrc.taggedMsg "RPC metadata initialized."

  -- We run a par computation that will not terminate to get the
  -- system up, running, and work-stealing:
  runMetaParIO (RemoteRsrc.initAction metadata RemoteRsrc.Slave)
	       RemoteRsrc.stealAction
	       (new >>= get)

  fail "RETURNED FROM runMetaParIO - THIS SHOULD NOT HAPPEN"

-- This is a blocking operation that waits until shutdown is complete.
shutdownDist :: IO ()
shutdownDist = do 
   uniqueTok <- randomIO
   RemoteRsrc.initiateShutdown uniqueTok
   RemoteRsrc.waitForShutdown  uniqueTok
