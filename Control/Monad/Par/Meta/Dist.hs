module Control.Monad.Par.Meta.Dist (
--    runPar
--  , runParIO
    runParDist
  , runParSlave
  , runParDistWithTransport
  , runParSlaveWithTransport
  , shutdownDist
  , Rem.longSpawn
  , WhichTransport(..)
  , module Control.Monad.Par.Meta
) where

import Control.Monad.Par.Meta
import qualified Control.Monad.Par.Meta.Resources.Remote as Rem
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment (getEnvironment)
import Data.List (lookup)
import Control.Monad (liftM)
import Control.Monad.Par.Meta.HotVar.IORef
import Control.Exception (catch, throw, SomeException)

import qualified Network.Transport     as T
import qualified Network.Transport.TCP as TCP
import qualified Network.Transport.Pipes as PT

import Prelude hiding (catch)
import System.Random (randomIO)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (getProcessID)
import Remote2.Reg (registerCalls)

import GHC.Conc

-- | Select from available transports or provide your own.  A custom
--   implementation is required to create a transport for each node
--   given only an indication of master or slave.  Notably, it is told
--   nothing about WHICH slave is being instantiated and must
--   determine that on its own.
data WhichTransport = 
    TCP 
  | Pipes 
--  | MPI 
  | Custom (Rem.InitMode -> IO T.Transport)

-- tries = 20
-- caps  = numCapabilities

ia metadata trans sa scheds = 
     do env <- getEnvironment        
	ml <- case lookup "MACHINE_LIST" env of 
	       Just str -> return (words str)
	       Nothing -> 
		 case lookup "MACHINE_LIST_FILE" env of 
		   Just fl -> liftM words $ readFile fl
  	  	   Nothing -> error$ "Remote resource: Expected to find machine list in "++
			             "env var MACHINE_LIST or file name in MACHINE_LIST_FILE."
        Rem.initAction metadata trans (Rem.Master$ map BS.pack ml) sa scheds
sa :: StealAction
sa = Rem.stealAction 

-- The default Transport is TCP:
runParDist mt = runParDistWithTransport mt TCP

runParDistWithTransport metadata trans comp = catch main hndlr 
 where 
   main = runMetaParIO (ia metadata (pickTrans trans)) sa comp
   hndlr e = do	hPutStrLn stderr $ "Exception inside runParDist: "++show e
		throw (e::SomeException)



-- When global initialization has already happened:
-- runParDistNested = runMetaParIO (ia Nothing) sa

runParSlave meta = runParSlaveWithTransport meta TCP

runParSlaveWithTransport metadata trans = do
  Rem.taggedMsg 2 "runParSlave invoked."

  -- We run a par computation that will not terminate to get the
  -- system up, running, and work-stealing:
  runMetaParIO (Rem.initAction metadata (pickTrans trans) Rem.Slave)
	       (\ x y -> do res <- Rem.stealAction x y; 
		            threadDelay (10 * 1000);
	                    return res)
	       (new >>= get)

  fail "RETURNED FROM runMetaParIO - THIS SHOULD NOT HAPPEN"

-- This is a blocking operation that waits until shutdown is complete.
shutdownDist :: IO ()
shutdownDist = do 
   uniqueTok <- randomIO
   Rem.initiateShutdown uniqueTok
   Rem.waitForShutdown  uniqueTok

--------------------------------------------------------------------------------
-- Transport-related inititialization:
--------------------------------------------------------------------------------

pickTrans trans = 
     case trans of 
       TCP   -> initTCP
       Pipes -> initPipes
--       MPI   ->
       Custom fn -> fn

initTCP :: Rem.InitMode -> IO T.Transport
initTCP mode = do 
    host <- Rem.hostName        
--    TCP.mkTransport $ TCP.TCPConfig T.defaultHints (BS.unpack host) control_port
    case mode of 
      Rem.Slave   -> do port <- breakSymmetry
                        TCP.mkTransport $ TCP.TCPConfig T.defaultHints host (show port)
      (Rem.Master _) -> TCP.mkTransport $ TCP.TCPConfig T.defaultHints host control_port


initPipes :: Rem.InitMode -> IO T.Transport
initPipes _ = PT.mkTransport

-- TODO: Make this configurable:
control_port :: String
control_port = "8098"

work_base_port :: Int
work_base_port = 8099


breakSymmetry :: IO Int
breakSymmetry =
  do mypid <- getProcessID
     -- Use the PID to break symmetry between multiple slaves on the same machine:
     let port  = work_base_port + fromIntegral mypid
	 port' = if port > 65535 
                 then (port `mod` (65535-8000)) + 8000
		 else port
     return port'
