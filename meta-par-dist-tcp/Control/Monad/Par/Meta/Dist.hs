{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
#ifndef DIST_SMP
module Control.Monad.Par.Meta.Dist 
#endif
(
    runParDist
  , runParSlave
  , runParDistWithTransport
  , runParSlaveWithTransport
  , shutdownDist
  , Rem.longSpawn
  , Rem.globalRPCMetadata
  , WhichTransport(..)
  , readTransport
  , module Control.Monad.Par.Meta
) where

import Control.Monad.Par.Class (get, new)
import Control.Monad.Par.Meta
import Control.Monad.Par.Meta.Resources.Debugging (dbgTaggedMsg)
import qualified Control.Monad.Par.Meta.Resources.Remote as Rem
import qualified Control.Monad.Par.Meta.Resources.Backoff as Bkoff

#ifdef DIST_SMP
#warning "Activating DistSMP via SMP Resource."
import qualified Control.Monad.Par.Meta.Resources.SMP   as Local
#else
import qualified Control.Monad.Par.Meta.Resources.SingleThreaded as Local
#endif

import qualified Data.ByteString.Char8 as BS
import System.Environment (getEnvironment)
import Data.Word
import Data.Monoid (mconcat)
import Control.Monad (void)
-- import Control.Monad.Par.Meta.HotVar.IORef
import Control.Exception (catch, throw, SomeException)

import qualified Network.Transport     as T
import qualified Network.Transport.TCP as TCP
import qualified Network.Transport.Pipes as PT

import System.Random (randomIO)
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (getProcessID)
import RPC.Reg (RemoteCallMetaData)
import Text.Printf

--------------------------------------------------------------------------------

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

readTransport :: String -> WhichTransport
readTransport "TCP"   = TCP
readTransport "Pipes" = Pipes
readTransport x       = error $ printf "Meta.Dist: unknown transport %s" x

--------------------------------------------------------------------------------
-- Read Backoff configuration from environment variables:

backoff_min :: Word64
backoff_min = unsafePerformIO$ do
	      env <- getEnvironment
              case lookup "BACKOFF_MIN" env of
                    Just s  -> do putStrLn$ "(!) Responding to BACKOFF_MIN="++s++" environment variable!"
                                  return (read s)
    	  	    Nothing -> return 1000

backoff_max :: Word64
backoff_max = unsafePerformIO$ do
	      env <- getEnvironment
              case lookup "BACKOFF_MAX" env of
                    Just s  -> do putStrLn$ "(!) Responding to BACKOFF_MAX="++s++" environment variable!"
                                  return (read s)
    	  	    Nothing -> return (100 * 1000)

--------------------------------------------------------------------------------
-- Startup and WorkSearches:

masterResource :: [RemoteCallMetaData]
               -> (Rem.InitMode -> IO T.Transport)
               -> Resource
masterResource metadata trans = 
  mconcat [ Local.mkResource
#ifdef DIST_SMP
              20
#endif
          , Rem.mkMasterResource metadata trans
          , Bkoff.mkResource backoff_min backoff_max
          ]

slaveResource :: [RemoteCallMetaData]
              -> (Rem.InitMode -> IO T.Transport)
              -> Resource
slaveResource metadata trans =
  mconcat [ Local.mkResource
#ifdef DIST_SMP
              20
#endif
          , Rem.mkSlaveResource metadata trans
          , Bkoff.mkResource backoff_min backoff_max
          ]

--------------------------------------------------------------------------------
-- Running and shutting down the distributed Par monad:

-- The default Transport is TCP:
runParDist :: [RemoteCallMetaData] -> Par a -> IO a
runParDist mt = runParDistWithTransport mt TCP

runParDistWithTransport :: [RemoteCallMetaData]
                        -> WhichTransport
                        -> Par a
                        -> IO a
runParDistWithTransport metadata trans comp = 
   do dbgTaggedMsg 1$ BS.pack$ "Initializing distributed Par monad with transport: "++ show trans
      Control.Exception.catch main hndlr 
 where 
   main = runMetaParIO (masterResource metadata (pickTrans trans)) comp
   hndlr e = do	BS.hPutStrLn stderr $ BS.pack $ "Exception inside runParDist: "++show e
		throw (e::SomeException)


-- Could have this for when global initialization has already happened:
-- runParDistNested = runMetaParIO (ia Nothing) sa

runParSlave :: [RemoteCallMetaData] -> IO ()
runParSlave meta = runParSlaveWithTransport meta TCP

runParSlaveWithTransport :: [RemoteCallMetaData] 
                         -> WhichTransport
                         -> IO ()
runParSlaveWithTransport metadata trans = do
  dbgTaggedMsg 2 (BS.pack "runParSlave invoked.")

  -- We run a par computation that will not terminate to get the
  -- system up, running, and work-stealing:
  void $ runMetaParIO (slaveResource metadata (pickTrans trans))
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

pickTrans :: WhichTransport -> Rem.InitMode -> IO T.Transport
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

min_port, max_port :: Int
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
