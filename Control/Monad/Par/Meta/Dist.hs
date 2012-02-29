module Control.Monad.Par.Meta.Dist (
--    runPar
--  , runParIO
    runParDist
  , runParSlave
  , runParDistWithTransport
  , runParSlaveWithTransport
  , shutdownDist
  , Rem.longSpawn
  , Rem.globalRPCMetadata
  , WhichTransport(..)
  , module Control.Monad.Par.Meta
) where

import Control.Monad.Par.Meta
import qualified Control.Monad.Par.Meta.Resources.Remote as Rem
import qualified Control.Monad.Par.Meta.Resources.Remote as RemoteRsrc
import qualified Control.Monad.Par.Meta.Resources.SingleThreaded as Single
import qualified Data.ByteString.Char8 as BS
import System.Environment (getEnvironment)
import Data.Char (ord)
import Data.List (lookup)
import Data.Monoid (mappend)
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

instance Show WhichTransport where
  show TCP = "TCP"
  show Pipes = "Pipes"
  show (Custom _) = "<CustomTransport>"

-- tries = 20
-- caps  = numCapabilities

masterInitAction metadata trans = IA ia
  where
    ia sa scheds = do
        env <- getEnvironment        
	ml <- case lookup "MACHINE_LIST" env of 
	       Just str -> return (words str)
	       Nothing -> 
		 case lookup "MACHINE_LIST_FILE" env of 
		   Just fl -> liftM words $ readFile fl
  	  	   Nothing -> error$ "Remote resource: Expected to find machine list in "++
			             "env var MACHINE_LIST or file name in MACHINE_LIST_FILE."
        runIA (Rem.initAction metadata trans (Rem.Master$ map BS.pack ml)) sa scheds
        runIA Single.initAction sa scheds

slaveInitAction metadata trans =
  Rem.initAction metadata trans Rem.Slave `mappend` Single.initAction

sa :: StealAction
sa = Single.stealAction `mappend` Rem.stealAction

--------------------------------------------------------------------------------

-- The default Transport is TCP:
runParDist mt = runParDistWithTransport mt TCP

runParDistWithTransport metadata trans comp = 
   do Rem.taggedMsg 1$ "Initializing distributed Par monad with transport: "++ show trans
      catch main hndlr 
 where 
   main = runMetaParIO (masterInitAction metadata (pickTrans trans)) sa comp
   hndlr e = do	hPutStrLn stderr $ "Exception inside runParDist: "++show e
		throw (e::SomeException)


-- When global initialization has already happened:
-- runParDistNested = runMetaParIO (ia Nothing) sa

runParSlave meta = runParSlaveWithTransport meta TCP

runParSlaveWithTransport metadata trans = do
  Rem.taggedMsg 2 "runParSlave invoked."

  -- We run a par computation that will not terminate to get the
  -- system up, running, and work-stealing:
  runMetaParIO (slaveInitAction metadata (pickTrans trans))
	       (SA $ \ x y -> do res <- runSA sa x y; 
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
      (Rem.Master _) -> TCP.mkTransport $ TCP.TCPConfig T.defaultHints host (show control_port)


initPipes :: Rem.InitMode -> IO T.Transport
initPipes _ = PT.mkTransport

-- TODO: Make this configurable:
control_port :: Int
control_port = min_port

work_base_port :: Int
work_base_port = min_port + 1 

min_port = 11000
max_port = 65535

breakSymmetry :: IO Int
breakSymmetry =
  do mypid <- getProcessID
     -- Use the PID to break symmetry between multiple slaves on the same machine:
     let port  = work_base_port + fromIntegral mypid
	 port' = if port > max_port
                 then (port `mod` (max_port - min_port)) + min_port
		 else port
     return port'
