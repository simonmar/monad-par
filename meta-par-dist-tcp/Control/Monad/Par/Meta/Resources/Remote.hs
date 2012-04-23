{-# LANGUAGE CPP, BangPatterns, PackageImports #-}
{-# LANGUAGE NamedFieldPuns, DeriveGeneric, ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fwarn-unused-imports #-}

-- Turn this on to do extra invariant checking
#define AUDIT_WORK
-- define TMPDBG

-- | Resource for Remote execution.

module Control.Monad.Par.Meta.Resources.Remote 
  ( sharedInit, defaultSteal, 
    initiateShutdown, waitForShutdown, 
    longSpawn, 
    InitMode(..),
    hostName,
    globalRPCMetadata,
    mkMasterResource,
    mkSlaveResource
  )  
 where

import "mtl" Control.Monad.Reader (ask)
import Control.Applicative    ((<$>))
import Control.Concurrent     (myThreadId, threadDelay, writeChan, readChan, newChan, Chan,
			       forkOS, threadCapability, ThreadId)
import Control.DeepSeq        (NFData)
import Control.Exception      (catch, SomeException)
import Control.Monad          (forM, forM_, liftM, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par.Meta.HotVar.IORef
import Data.Typeable          (Typeable)
import Data.IORef             (writeIORef, readIORef, newIORef, IORef)
import qualified Data.IntMap as IntMap
import qualified Data.Map    as M
import Data.Int               (Int64)
import Data.Word              (Word8)
import Data.Maybe             (fromMaybe)
import Data.Char              (isSpace)
import qualified Data.ByteString.Char8 as BS
-- Attempting to use a real concurrent data structure here:
#ifdef MSQUEUE
import Data.Concurrent.Queue.MichaelScott as R
import Data.Concurrent.Queue.MichaelScott.DequeInstance
#else
import Data.Concurrent.Deque.Reference as R
import Data.Concurrent.Deque.Reference.DequeInstance -- Populate the type family with instances.
#endif

import Data.Concurrent.Deque.Class     as DQ
import Data.List as L
import Data.List.Split    (splitOn)
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Data.Serialize (encode, decode, Serialize)
import qualified Data.Serialize as Ser
-- import Data.Serialize.Derive (deriveGet, derivePut)
import GHC.Generics       (Generic)
import System.Environment (getEnvironment)
import System.IO          (hFlush, stdout, stderr)
import System.IO.Unsafe   (unsafePerformIO)
import System.Process     (readProcess)
import System.Directory   (removeFile, doesFileExist, renameFile)
import System.Random      (randomIO)

import Control.Monad.Par.Class (new, put_)
import Control.Monad.Par.Meta.Resources.Debugging
   (dbgTaggedMsg, dbgDelay, dbgCharMsg, taggedmsg_global_mode)
import Control.Monad.Par.Meta (forkWithExceptions, Sched(Sched,no,ivarUID),
			       IVar, Par, Startup(..), WorkSearch(WS), Resource(..), GlobalState)
import qualified Network.Transport     as T
import RPC.Closure  (Closure(Closure))
import RPC.Encoding (Payload, Serializable, serialDecodePure)
import qualified RPC.Reg as Reg
import GHC.Conc (numCapabilities)

----------------------------------------------------------------------------------------------------
--                                              TODO                                              --
----------------------------------------------------------------------------------------------------

--   * Add configurability for constants below.
--   * Get rid of myTransport -- just use the peerTable
--   * Rewrite serialization using putWordhost

-----------------------------------------------------------------------------------
-- Data Types
-----------------------------------------------------------------------------------

-- | For now it is the master's job to know the machine list:
data InitMode = Master MachineList | Slave
-- | The MachineList may contain duplicates!!
type MachineList = [PeerName] 

type NodeID = Int

-- Format a nodeID:
showNodeID :: Int -> BS.ByteString
showNodeID n = "<Node"+++ sho n+++">"
(+++) :: Monoid a => a -> a -> a
(+++) = mappend

sho :: Show a => a -> BS.ByteString
sho = BS.pack . show

-- Control messages are for starting the system up and communicating
-- between slaves and the master.  

-- We group all messages under one datatype, though often these
-- messages are used in disjoint phases and could be separate
-- datatypes.
data Message = 
   -- First CONTROL MESSAGES:   
   ----------------------------------------
     AnnounceSlave { name   :: PeerName,
		     toAddr :: BS.ByteString }

     -- The master informs the slave of its index in the peerTable:
   | MachineListMsg (Int, PeerName) !Int MachineList

     -- The worker informs the master that it has connected to all
     -- peers (eager connection mode only).
   | ConnectedAllPeers

     -- The master grants permission to start stealing.
     -- No one had better send the master steal requests before this goes out!!
   | StartStealing
     
   | ShutDown !Token -- A message from master to slave, "exit immediately":
   | ShutDownACK    -- "I heard you, shutting down gracefully"

   -- Second, WORK MESSAGES:   
   ----------------------------------------
   -- Work messages are for getting things done (stealing work,
   -- returning results).
   ----------------------------------------

     -- | A message signifying a steal request and including the NodeID of
     --   the sender:
   | StealRequest !NodeID
   | StealResponse !NodeID (IVarId, Closure Payload)

     -- The result of a parallel computation, evaluated remotely:
   | WorkFinished !NodeID !IVarId Payload

  deriving (Show, Generic, Typeable)


-- | Unique (per-machine) identifier for 'IVar's
type IVarId = Int

type ParClosure a = (Par a, Closure (Par a))


-----------------------------------------------------------------------------------
-- Global constants:
-----------------------------------------------------------------------------------

master_addr_file :: String
master_addr_file = "master_control.addr"

mkAddrFile :: Show a => BS.ByteString -> a -> String
mkAddrFile machine n = "./"++show n++"_"++ BS.unpack machine  ++ "_source.addr" 

-- | Flag: establish all-to-all connections BEFORE beginning any real computation.
eager_connections :: Bool
eager_connections = False
-- Other temporary toggles:
-- define SHUTDOWNACK
-- define KILL_WORKERS_ON_SHUTDOWN


--------------------------------------------------------------------------------
-- Global Mutable Structures 
--------------------------------------------------------------------------------

-- TODO: Several of these things (myNodeID, masterID, etc) could
-- become immutable, packaged in a config record, and be passed around
-- to the relavant places.


-- An item of work that can be executed either locally, or stolen
-- through the network.
data LongWork = LongWork {
     localver  :: Par (),
     stealver  :: Maybe (IVarId, Closure Payload)
  }

{-# NOINLINE longQueue #-}
#ifdef MSQUEUE
longQueue :: R.LinkedQueue LongWork
#else
longQueue :: DQ.ConcQueue LongWork
#endif
longQueue = unsafePerformIO $ R.newQ

{-# NOINLINE peerTable #-}
-- Each peer is either connected, or not connected yet.
type PeerName = BS.ByteString
peerTable :: HotVar (V.Vector (Maybe (PeerName, T.SourceEnd)))
peerTable = unsafePerformIO $ newHotVar (error "global peerTable uninitialized")

{-# NOINLINE machineList #-}
machineList :: IORef [PeerName]
machineList = unsafePerformIO$ newIORef (error "global machineList uninitialized")

{-# NOINLINE myTransport #-}
myTransport :: IORef T.Transport
myTransport = unsafePerformIO$ newIORef (error "global myTransport uninitialized")

{-# NOINLINE myNodeID #-}
myNodeID :: HotVar Int
myNodeID = unsafePerformIO$ newIORef (error "uninitialized global 'myNodeID'")

{-# NOINLINE masterID #-}
masterID :: HotVar Int
masterID = unsafePerformIO$ newIORef (error "uninitialized global 'masterID'")


{-# NOINLINE isMaster #-}
isMaster :: HotVar Bool
isMaster = unsafePerformIO$ newIORef False

{-# NOINLINE globalRPCMetadata #-}
globalRPCMetadata :: IORef Reg.Lookup
globalRPCMetadata = unsafePerformIO$ newIORef (error "uninitialized 'globalRPCMetadata'")

-- HACK: Assume the init action is invoked from the main program thread.
-- We use this to throw exceptions that will really exit the process.
{-# NOINLINE mainThread #-}
mainThread :: IORef ThreadId
mainThread = unsafePerformIO$ newIORef (error "uninitialized global 'mainThread'")

{-# NOINLINE remoteIvarTable #-}
remoteIvarTable :: HotVar (IntMap.IntMap (Payload -> Par ()))
remoteIvarTable = unsafePerformIO$ newHotVar IntMap.empty

type Token = Int64
{-# NOINLINE shutdownChan #-}
shutdownChan :: Chan Token
shutdownChan = unsafePerformIO $ newChan

-----------------------------------------------------------------------------------
-- Debugging Helpers:
-----------------------------------------------------------------------------------

measureIVarTable :: IO (Int, BS.ByteString)
measureIVarTable = do
  tbl <- readIORef remoteIvarTable
  let size = IntMap.size tbl
  return (size, " (Outstanding unreturned work " +++ sho size +++ ", ids:"+++
	          sho (take 5 $ IntMap.keys tbl)+++" )")

-----------------------------------------------------------------------------------
-- Misc Helpers:
-----------------------------------------------------------------------------------

decodeMsg :: Serialize a => BS.ByteString -> a
decodeMsg str = 
  case decode str of
    Left _  -> errorExitPure$ "ERROR: decoding message from: " ++ show str
    Right x -> x

-- forkDaemon = forkIO
forkDaemon :: String -> IO () -> IO ThreadId
forkDaemon name action = 
   forkWithExceptions forkOS ("Daemon thread ("++name++")") action
--   forkOS $ catch action
-- 		 (\ (e :: SomeException) -> do
-- 		  hPutStrLn stderr $ "Caught Error in Daemon thread ("++name++"): " ++ show e
-- 		 )

hostName :: IO String
hostName = trim <$>
--    readProcess "uname" ["-n"] ""
    readProcess "hostname" [] ""
 where 
  -- | Trim whitespace from both ends of a string.
  trim :: String -> String
  trim = f . f
     where f = reverse . dropWhile isSpace


-- | Send to a particular node in the peerTable:
sendTo :: NodeID -> BS.ByteString -> IO ()
sendTo ndid msg = do
    (_,conn) <- connectNode ndid
    T.send conn [msg]

-- | We don't want anyone to try to use a file that isn't completely written.
--   Note, this needs further thought for use with NFS...
atomicWriteFile :: FilePath -> BS.ByteString -> IO ()
atomicWriteFile file contents = do 
   rand :: Int64 <- randomIO
   let tmpfile = "./tmpfile_"++show rand
   BS.writeFile tmpfile contents
   renameFile tmpfile file


-- | This assumes a "Payload closure" as produced by makePayloadClosure below.
deClosure :: Closure Payload -> IO (Par Payload)
-- deClosure :: (Typeable a) => Closure a -> IO (Maybe a)
deClosure (Closure ident payload) = do 
  dbgTaggedMsg 5$ "Declosuring : "+++ sho ident +++ " type "+++ sho payload
  lkp <- readIORef globalRPCMetadata
  case Reg.getEntryByIdent lkp ident of 
    Nothing -> fail$ "deClosure: failed to deserialize closure identifier: "++show ident
    Just fn -> return (fn payload)

-- TEMP FIXME: INLINING THIS HERE:
makePayloadClosure :: Closure a -> Maybe (Closure Payload)
makePayloadClosure (Closure name arg) = 
                case isSuffixOf "__impl" name of
                  False -> Nothing
                  True -> Just $ Closure (name++"Pl") arg

-- | Try to exit the whole process and not just the thread.
errorExit :: String -> IO a
-- TODO: Might be better to just call "kill" on our own PID:
errorExit str = do
   printErr    $ "ERROR: "++str 
   dbgTaggedMsg 0 $ BS.pack $ "ERROR: "++str 

-- The idea with this debugging hack is to prevent ANYTHING that could be closing connections:
#ifdef TMPDBG
   diverge
#else
   closeAllConnections
#endif
   printErr$ "Connections closed, now exiting process."
   exitProcess 1

printErr :: String -> IO ()
printErr = BS.putStrLn . BS.pack 

foreign import ccall "exit" c_exit :: Int -> IO ()

-- | Exit the process successfully.
exitSuccess :: IO a
exitSuccess = do 
   dbgTaggedMsg 1 "  EXITING process with success exit code."
#ifdef TMPDBG
   diverge
#else
   closeAllConnections
#endif
   dbgTaggedMsg 1 "   (Connections closed, now exiting for real)"
   exitProcess 0

-- TODO: remove before release
--diverge = do putStr "!?"; threadDelay 100; diverge
diverge :: IO a
diverge = do dbgCharMsg 0 "!?" "Purposefully diverging instead of exiting for this experiment..."
	     threadDelay 200
	     diverge

exitProcess :: Int -> IO a
exitProcess code = do
   -- Cleanup: 
   hFlush stdout
   hFlush stderr
   -- Hack... pause for a bit...
   threadDelay (100*1000)
   ----------------------------------------
   -- Option 1: Async exception
   -- tid <- readIORef mainThread
   -- throwTo tid (ErrorCall$ "Exiting with code: "++show code)
   --   NOTE ^^ - this was having problems not being serviced.

   -- Option 2: kill:
   --      mypid <- getProcessID
   --      system$ "kill "++ show mypid

   -- Option 3: FFI call:
   c_exit code

   putStrLn$ "SHOULD NOT SEE this... process should have exited already."
   return (error "Should not see this.")

errorExitPure :: String -> a
errorExitPure str = unsafePerformIO$ errorExit str

closeAllConnections :: IO ()
closeAllConnections = do
  pt <- readHotVar peerTable
  V.forM_ pt $ \ entry -> 
    case entry of 
      Nothing -> return ()
      Just (_,targ) -> T.closeSourceEnd targ
  trans <- readIORef myTransport
  T.closeTransport trans
    
-----------------------------------------------------------------------------------
-- Initialize & Establish workers.
-----------------------------------------------------------------------------------
--
-- This initialization and discovery phase could be factored into a
-- separate module.  At the end of the discovery process the global
-- structure is initialized with a table of connections to all other
-- peer nodes.

-- (However, these connections may or may not actually be connected.
-- Lazy connection is permissable.)

-- | Initialize one part of the global state.
initPeerTable :: [PeerName] -> IO ()
initPeerTable ms = do
     writeIORef  machineList ms
     lines <- forM (zip [0::Int ..] ms) $ \ (i,m) -> 
	        return ("  "++show i++": "++ BS.unpack m)
     dbgTaggedMsg 1 $ BS.pack $ "PeerTable:\n" ++ unlines lines
     writeHotVar peerTable (V.replicate (length ms) Nothing)


-- | Connect to a node if there is not already a connection.  Return
--   the name and active connection.
connectNode :: Int -> IO (PeerName, T.SourceEnd)
connectNode ind = do
  pt <- readHotVar peerTable
  let entry = pt V.! ind
  case entry of 
    Just x  -> return x
    Nothing -> do 
      ml <- readIORef machineList
      let name  = ml !! ind
          file = mkAddrFile name ind

      toPeer <- waitReadAndConnect ind file
      let entry = (name, toPeer)
      modifyHotVar_ peerTable (\pt -> pt V.// [(ind,Just entry)])
      return entry

-- | Wait until a file appears, read an addr from it, and connect.
waitReadAndConnect :: Int -> FilePath -> IO T.SourceEnd
waitReadAndConnect _ file = do 
      dbgTaggedMsg 2$ BS.pack $ "    Establishing connection, reading file: "++file
      transport <- readIORef myTransport
      let 
          loop firstTime = do
	  b <- doesFileExist file 
	  if b then BS.readFile file
	   else do if firstTime 
                    then dbgTaggedMsg  2  (BS.pack$ "  File "++ file ++" does not exist yet....")
		    else dbgCharMsg 2 "." (BS.pack$ "  File "++ file ++" still does not exist...")
		   threadDelay (10*1000) -- Wait between file system checks.
		   loop False
      bstr <- loop True
      addr <- case T.deserialize transport bstr of 
 	        Nothing -> fail$ " [distmeta] Garbage addr in file: "++ file
		Just a  -> return a
      conn <- T.connect addr
      dbgTaggedMsg 2 "     ... Connected."
      return conn

extendPeerTable :: Int -> (PeerName, T.SourceEnd) -> IO ()
extendPeerTable id entry = 
  modifyHotVar_ peerTable (V.modify (\v -> MV.write v id (Just entry)))

--------------------------------------------------------------------------------


-----------------------------------------------------------------------------------
-- Main scheduler components (init & steal)
-----------------------------------------------------------------------------------

mkMasterResource :: [Reg.RemoteCallMetaData]
                 -> (InitMode -> IO T.Transport)
                 -> Resource
mkMasterResource metadata trans =
  Resource (masterInit metadata trans) defaultSteal

mkSlaveResource :: [Reg.RemoteCallMetaData]
                -> (InitMode -> IO T.Transport)
                -> Resource
mkSlaveResource metadata trans =
  Resource (sharedInit metadata trans Slave) defaultSteal

masterInit :: [Reg.RemoteCallMetaData]
           -> (InitMode -> IO T.Transport)
           -> Startup
masterInit metadata trans = St st
  where
    st ws scheds = do
        env <- getEnvironment
        host <- hostName 
	ml <- case lookup "MACHINE_LIST" env of 
	       Just str -> return (words str)
	       Nothing -> 
		 case lookup "MACHINE_LIST_FILE" env of 
		   Just fl -> liftM words $ readFile fl
  	  	   Nothing -> do BS.putStrLn$BS.pack$ "WARNING: Remote resource: Expected to find machine list in "++
			                              "env var MACHINE_LIST or file name in MACHINE_LIST_FILE."
                                 return [host]
        runSt (sharedInit metadata trans (Master$ map BS.pack ml)) ws scheds


sharedInit :: [Reg.RemoteCallMetaData] -> (InitMode -> IO T.Transport) -> InitMode -> Startup
  -- For now we bake in assumptions about being able to SSH to the machine_list:

sharedInit metadata initTransport (Master machineList) = St st
  where
    st _ schedMap = do 
     dbgTaggedMsg 2 "Initializing master..."

     -- Initialize the peerTable now that we know how many nodes there are:
     initPeerTable machineList

     -- Initialize the transport layer:
     transport <- initTransport (Master machineList)

     -- Write global mutable variables:
     ----------------------------------------
     host <- BS.pack <$> hostName
     let myid = nameToID host 0 machineList
     dbgTaggedMsg 3 $ "Master node id established as: "+++ showNodeID myid
     writeIORef myNodeID myid
     writeIORef masterID myid
     writeIORef isMaster True
     writeIORef myTransport transport
     myThreadId >>= writeIORef mainThread
     writeIORef globalRPCMetadata (Reg.registerCalls metadata)
     dbgTaggedMsg 3 "RPC metadata initialized."
     ----------------------------------------


     (sourceAddr, targetEnd) <- T.newConnection transport
     -- Hygiene: Clear files storing slave addresses:
     forM_ (zip [0::Int ..] machineList) $ \ (ind,machine) -> do
        let file = mkAddrFile machine ind
        b <- doesFileExist file
        when b $ removeFile file

     -- Write a file with our address enabling slaves to find us:
     atomicWriteFile master_addr_file       $ T.serialize sourceAddr
     -- We are also a worker, so we write the file under our own name as well:
     atomicWriteFile (mkAddrFile host myid) $ T.serialize sourceAddr

     -- Remember how to talk to ourselves (as a worker):
     -- extendPeerTable myid (host, sourceAddr)

     -- Connection Listening Loop:
     ----------------------------------------
     -- Every time we, the master, receive a connection on this port
     -- it indicates a new slave starting up.
     -- As the master we eagerly set up connections with everyone:
     let
         slaveConnectLoop 0    _                = return ()
	 slaveConnectLoop iter alreadyConnected = do
          strs <- T.receive targetEnd 
          let str = BS.concat strs
              msg = decodeMsg str
          case msg of
            AnnounceSlave{name,toAddr} -> do 
	      dbgTaggedMsg 3$ "Master: register new slave, name: "+++ name
	      dbgTaggedMsg 4$ "  Full message received from slave: "+++ sho str

	      -- Deserialize each of the sourceEnds received by the slaves:
	      case T.deserialize transport toAddr of
		Nothing         -> fail "Garbage message from slave!"
		Just sourceAddr -> do 
		  srcEnd <- T.connect sourceAddr
		  let (id, alreadyConnected') = updateConnected name alreadyConnected machineList
		  -- Convention: send slaves the machine list, and tell it what its ID is:
		  T.send srcEnd [encode$ MachineListMsg (myid,host) id machineList]

		  dbgTaggedMsg 4$ "Sent machine list to slave "+++showNodeID id+++": "+++ BS.unwords machineList

		  -- Write the file to communicate that the machine is
		  -- online and set up a way to contact it:
		  let filename = mkAddrFile name id

		  atomicWriteFile filename toAddr
		  dbgTaggedMsg 3$ BS.pack $ "  Wrote file to signify slave online: " ++ filename

		  -- Keep track of the connection for future communications:
		  dbgTaggedMsg 4$ "  Extending peerTable with node ID "+++ sho id
		  extendPeerTable id (name, srcEnd)

		  slaveConnectLoop (iter-1) alreadyConnected' 

            othermsg -> errorExit$ "Received unexpected message when expecting AnnounceSlave: "++show othermsg            
     ----------------------------------------
     dbgTaggedMsg 3 "  ... waiting for slaves to connect."
     let allButMe = if (elem host machineList) 
		    then delete host machineList
		    else error$ "Could not find master domain name " ++ (BS.unpack host)++
			        " in machine list:\n  "++ unwords (map BS.unpack machineList)
     slaveConnectLoop (length allButMe) (M.singleton host 1)
     dbgTaggedMsg 2 "  ... All slaves announced themselves."

     ----------------------------------------
     -- Possibly wait for slaves to get all their peer connections up:
     when eager_connections $ do 
       dbgTaggedMsg 2 "  Waiting for slaves to bring up all N^2 mutual connections..."
       forM_ (zip [0..] machineList) $ \ (ndid,name) -> 
         unless (ndid == myid) $ do 
          msg <- decodeMsg <$> BS.concat <$> T.receive targetEnd 
          case msg of 
   	     ConnectedAllPeers -> putStrLn$ "  "++ show ndid ++ ", "
	                           ++ (BS.unpack name) ++ ": peers connected."
	     msg -> errorExit$ "Expected message when expecting ConnectAllPeers:"++ show msg

     dbgTaggedMsg 2 "  All slaves ready!  Launching receiveDaemon..."
     void $ forkDaemon "ReceiveDaemon"$ receiveDaemon targetEnd schedMap
       
     ----------------------------------------
     -- Allow slaves to proceed past the barrier:     
     forM_ (zip [0..] machineList) $ \ (ndid,_) -> 
       unless (ndid == myid) $ 
	 sendTo ndid (encode StartStealing)

     ----------------------------------------
     dbgTaggedMsg 3 "Master initAction returning control to scheduler..."
     return ()
    -- Annoying book-keeping:  Keep track of how many we've seen
    -- and make sure that the client that actually connect match
    -- the machineList:
    updateConnected name map machineList =  
      let map' = M.insertWith' (+) name 1 map 
	  cnt  = map' M.! name
      in (nameToID name (cnt-1) machineList,
	  map') 

    -- Look up a host's position in the MachineList, skipping the first N
    nameToID :: PeerName -> Int -> [PeerName] -> Int
    nameToID bs1 skip bsls = 
      let indices = findIndices (\ bs2 -> bs1 == bs2 
    	 	                -- Hack: handle foo instead of foo.domain.com:
				|| basename bs1 == basename bs2) bsls
      in if length indices > skip 
	 then indices !! skip
	 else error$ "nameToID: hostname not in [or not included enough times] in machine list: "++ (BS.unpack bs1)

    basename bs = BS.pack$ head$ splitOn "." (BS.unpack bs)

------------------------------------------------------------------------------------------
sharedInit metadata initTransport Slave = St st
  where
    st _ schedMap = do 
     dbgTaggedMsg 2 "Init slave: creating connection... " 
     host <- BS.pack <$> commonInit metadata initTransport
     writeIORef taggedmsg_global_mode "_S"

     transport <- readIORef myTransport
     (mySourceAddr, myInbound) <- T.newConnection transport

     dbgTaggedMsg 3$ "  Source addr created: " +++ sho (T.serialize mySourceAddr)

     let 
         waitMasterFile = do  
           -- Loop until we see the master's file.
           -- For now: Assume shared filesystem:
           e <- doesFileExist master_addr_file
           if e then return ()
                else do threadDelay (10*1000) -- Wait between checking filesystem.
		        waitMasterFile
         ----------------------------------------
         doMasterConnection = do
             dbgTaggedMsg 3$ BS.pack $ "Found master's address in file: "++ master_addr_file
             bstr <- BS.readFile master_addr_file
	     case T.deserialize transport bstr of 
	       Nothing -> fail$ " [distmeta] Garbage message in master file: "++ master_addr_file
   	       Just masterAddr -> do 
		 toMaster <- T.connect masterAddr
		 -- Convention: connect by sending name and reverse connection:
		 T.send toMaster [encode$ AnnounceSlave host (T.serialize mySourceAddr)]

                 dbgTaggedMsg 3 "Sent name and addr to master "
	         _machines_bss <- T.receive myInbound
                 let MachineListMsg (masterId,masterName) myid machines = decodeMsg (BS.concat _machines_bss)
                 dbgTaggedMsg 2$ "Received machine list from master: "+++ BS.unwords machines
		 initPeerTable machines
		 writeIORef myNodeID myid
                 writeIORef masterID masterId
                 -- Since we already have a connection to the master we store this for later:
		 extendPeerTable masterId (masterName, toMaster)

                 -- Further we already have our OWN connection, so we put this in the table for consistency:
--		 extendPeerTable myid (host,myInbound)

                 return (masterId, machines)
         ----------------------------------------
         establishALLConections (masterId, machines) = do
	     dbgTaggedMsg 2 "  Proactively connecting to all peers..."
	     myid <- readIORef myNodeID 
	     forM_ (zip [0..] machines) $ \ (ndid,_) -> 
		unless (ndid == myid) $ do 
		  void $ connectNode ndid
 --			dbgTaggedMsg$ "    "++show ndid++": "++BS.unpack name ++" connected."
		  return ()
	     dbgTaggedMsg 2 "  Fully connected, notifying master."
	     sendTo masterId (encode ConnectedAllPeers)
         ----------------------------------------
         waitBarrier = do 
	     -- Don't proceed till we get the go-message from the Master:
	     msg <- T.receive myInbound
	     case decodeMsg (BS.concat msg) of 
	       StartStealing -> dbgTaggedMsg 1$ "Received 'Go' message from master.  BEGIN STEALING."
	       msg           -> errorExit$ "Expecting StartStealing message, received: "++ show msg              

     -- These are the key steps:
     waitMasterFile
     pr <- doMasterConnection
     when eager_connections (establishALLConections pr)
     waitBarrier

     void $ forkDaemon "ReceiveDaemon"$ receiveDaemon myInbound schedMap
     dbgTaggedMsg 3$ "Slave initAction returning control to scheduler..."

     return ()


-- TODO - FACTOR OUT OF MASTER CASE AS WELL:
-- | Common pieces factored out from the master and slave initializations.
commonInit :: [Reg.RemoteCallMetaData]
           -> (InitMode -> IO T.Transport)
           -> IO String
commonInit metadata initTransport = do 
  
     writeIORef globalRPCMetadata (Reg.registerCalls metadata)
     dbgTaggedMsg 3$ "RPC metadata initialized."

     host <- hostName
     transport <- initTransport Slave
     writeIORef myTransport transport

     -- ASSUME init action is called from main thread:
     myThreadId >>= writeIORef mainThread

     return host

--------------------------------------------------------------------------------

-- We initiate shutdown of the remote Monad par scheduling system by
-- sending a message to our own receiveDaemon.  This can only be
-- called on the master node.
initiateShutdown :: Token -> IO ()
initiateShutdown token = do 
  master <- readHotVar isMaster  
  myid   <- readIORef myNodeID
  unless master $ errorExit$ "initiateShutdown called on non-master node!!"
  sendTo myid (encode$ ShutDown token)
  -- TODO: BLOCK Until shutdown is successful.

-- Block until the shutdown process is successful.
waitForShutdown :: Token -> IO ()
waitForShutdown token = do 
   t <- readChan shutdownChan 
   unless (t == token) $ 
     errorExit$ "Unlikely data race occured!  Two separet distributed Par instances tried to shutdown at the same time." 
   return ()

-- The master tells all workers to quit.
masterShutdown :: Token -> T.TargetEnd -> IO ()
#ifdef SHUTDOWNACK
masterShutdown token targetEnd = do
#else
masterShutdown token _ = do
#endif
   mLs  <- readIORef machineList
   myid <- readIORef myNodeID
   ----------------------------------------  
   forM_ (zip [0..] mLs) $ \ (ndid,_) -> 
     unless (ndid == myid) $ do 
      sendTo ndid (encode$ ShutDown token)
   ----------------------------------------  
#ifdef SHUTDOWNACK
   dbgTaggedMsg 1$ "Waiting for ACKs that all slaves shut down..."
   let loop 0 = return ()
       loop n = do msg <- T.receive targetEnd
		   case decodeMsg (BS.concat msg) of
		     ShutDownACK -> loop (n-1)
		     other -> do 
                                 dbgTaggedMsg 4$ "Warning: received other msg while trying to shutdown.  Might be ok: "++show other
			         loop n
   loop (length mLs - 1)
   dbgTaggedMsg 1$ "All nodes shut down successfully."
#endif
   writeChan shutdownChan token

-- TODO: Timeout.


workerShutdown :: GlobalState -> IO ()
workerShutdown schedMap = do
   dbgTaggedMsg 1$ "Shutdown initiated for worker."
#ifdef SHUTDOWNACK
   mid <- readIORef masterID
   sendTo mid (encode ShutDownACK)
#endif
   -- Because we are completely shutting down the process we are at
   -- liberty to kill all worker threads here:
#ifdef KILL_WORKERS_ON_SHUTDOWN
   forM_ [0 .. size schedMap] $ \ i -> do
-- TODO:
-- FIXME FIXME FIXME FIXME FIXME 
     Just (Sched{tids}) <- schedMap ! i 
-- FIXME FIXME FIXME FIXME FIXME 
--     set <- readHotVar tids
     set <- modifyHotVar tids (\set -> (Set.empty,set))
     mapM_ killThread (Set.toList set) 
   dbgTaggedMsg 1$ "  Killed all Par worker threads."
#endif
   exitSuccess
   
-- Kill own pid:
-- shutDownSelf


--------------------------------------------------------------------------------

defaultSteal :: WorkSearch
defaultSteal = WS ws
  where 
    ws Sched{no} _ = do
      dbgDelay "stealAction"
      -- First try to pop local work:
      x <- R.tryPopR longQueue
      case x of 
        Just (LongWork{localver}) -> do
          dbgTaggedMsg 3$ "stealAction: worker number "+++sho no+++" found work in own queue."
          return (Just localver)
        Nothing -> raidPeer

    pickVictim myid = do
      pt   <- readHotVar peerTable
      let len = V.length pt
          loop = do 
            n :: Int <- randomIO 
 	    let ind = n `mod` len
 	    if ind == myid 
             then pickVictim myid
	     else return ind
      loop 

    raidPeer = do
      myid <- readHotVar myNodeID
      ind  <- pickVictim myid
      (_,str) <- measureIVarTable
      dbgTaggedMsg 4$ ""+++ showNodeID myid+++" Attempting steal from Node ID "
                      +++showNodeID ind+++"  "+++ str
--     (_,conn) <- connectNode ind 
--     T.send conn [encode$ StealRequest myid]
      sendTo ind (encode$ StealRequest myid)

      -- We have nothing to return immediately, but hopefully work will come back later.
      return Nothing

--------------------------------------------------------------------------------

-- | Spawn a parallel subcomputation that can happen either locally or remotely.
longSpawn (local, clo) = do
  let pclo = fromMaybe (errorExitPure "Could not find Payload closure")
                     $ makePayloadClosure clo
  Sched{no, ivarUID} <- ask

  iv <- new
  liftIO$ do
    dbgTaggedMsg 5$ " Serialized closure: " +++ sho clo
    (cap, _) <- (threadCapability =<< myThreadId)
    dbgTaggedMsg 5$ " [cap "+++ sho cap +++"] longSpawn'ing computation..." 

    -- Create a unique identifier for this IVar that is valid for the
    -- rest of the current run:
--    ivarid <- hashUnique <$> newUnique -- This is not actually safe.  Hashes may collide.

    cntr <- modifyHotVar ivarUID (\ !n -> (n+1,n) )
    -- This should be guaranteed to be unique:
    let ivarid = numCapabilities * cntr + no

    let ivarCont payl = case serialDecodePure payl of 
			  Just x  -> put_ iv x
			  Nothing -> errorExitPure$ "Could not decode payload: "++ show payl

    -- Update the table with a Par computation that can write in the result.
    modifyHotVar_ remoteIvarTable (IntMap.insert ivarid ivarCont)

    -- Pushing new spawned work should go on the right end of the queue:
#ifdef MSQUEUE
    -- For a basic queue it's only push left, pop right:
    R.pushL longQueue 
#else
    R.pushR longQueue 
#endif
       (LongWork{ stealver= Just (ivarid,pclo),
		  localver= do x <- local
                               liftIO$ do 
                                  dbgTaggedMsg 4 $ "Executed LOCAL version of longspawn.  "+++
		                                   "Filling & unregistering ivarid "+++sho ivarid
		                  modifyHotVar_ remoteIvarTable (IntMap.delete ivarid)
		               put_ iv x
		})

  return iv


--------------------------------------------------------------------------------

-- | Receive steal requests from other nodes.  This runs in a loop indefinitely.
receiveDaemon :: T.TargetEnd -> HotVar GlobalState -> IO ()
receiveDaemon targetEnd schedMap = 
  do myid <- readIORef myNodeID
     rcvLoop myid
 where 
  rcvLoop myid = do

   dbgDelay "receiveDaemon"
   (_,outstanding) <- measureIVarTable
   dbgTaggedMsg 4$ "[rcvdmn] About to do blocking rcv of next msg... "+++ outstanding

   -- Do a blocking receive to process the next message:
   bss  <- if False
	   then Control.Exception.catch 
                      (T.receive targetEnd)
		      (\ (_::SomeException) -> do
		       printErr$ "Exception while attempting to receive message in receive loop."
		       exitSuccess
		      )
	   else T.receive targetEnd
   dbgTaggedMsg 4$ "[rcvdmn] Received "+++ sho (BS.length (BS.concat bss)) +++" byte message..."

   case decodeMsg (BS.concat bss) of 
     StealRequest ndid -> do
       dbgCharMsg 3 "!" ("[rcvdmn] Received StealRequest from: "+++ showNodeID ndid)

       -- There are no "peek" operations currently.  Instead assuming pushL:
#ifdef MSQUEUE
       p <- R.tryPopR longQueue -- Queue's only popR
#else
       p <- R.tryPopL longQueue -- (WS)Deques popL
#endif
       case p of 
	 Just (LongWork{stealver= Just stealme}) -> do 
	   dbgTaggedMsg 2 "[rcvdmn]   StealRequest: longwork in stock, responding with StealResponse..."
	   sendTo ndid (encode$ StealResponse myid stealme)

          -- TODO: FIXME: Dig deeper into the queue to look for something stealable:
	 Just x -> do 
	    dbgTaggedMsg 2  "[rcvdmn]   StealRequest: Uh oh!  The bottom thing on the queue is not remote-executable.  Requeing it."
--            R.pushL longQueue x
-- NEW_REQUE_STRATEGY: Get the thing on the left out of our way!  Move it to the other end.  Assumes FULL concurrent Deque:
            R.pushR longQueue x
	 Nothing -> do
	    dbgTaggedMsg 4  "[rcvdmn]   StealRequest: No work to service request.  Not responding."
            return ()
       rcvLoop myid

     StealResponse fromNd pr@(ivarid,pclo) -> do
       dbgTaggedMsg 3$ "[rcvdmn] Received Steal RESPONSE from "+++showNodeID fromNd+++" "+++ sho pr
       loc <- deClosure pclo
       -- Here the policy is to execute local work (woken
       -- continuations) using the SAME work queue.  We thus have
       -- heterogeneous entries in that queue.

-- NEW_REQUE_STRATEGY: Push everything NONSTEALABLE on the right.  We
-- COULD make this stealable, but we worry about ping-poinging.
       R.pushR longQueue 
--       R.pushL longQueue 
	    (LongWork { stealver = Nothing,
			localver = do 
				     liftIO$ dbgTaggedMsg 1 $ "[rcvdmn] RUNNING STOLEN PAR WORK "
				     payl <- loc
				     liftIO$ dbgTaggedMsg 1 $ "[rcvdmn]   DONE running stolen par work."
				     liftIO$ sendTo fromNd (encode$ WorkFinished myid ivarid payl)
				     return ()
		       })
       rcvLoop myid

     WorkFinished fromNd ivarid payload -> do
       dbgTaggedMsg 2$ "[rcvdmn] Received WorkFinished from "+++showNodeID fromNd+++
		    " ivarID "+++sho ivarid+++" payload: "+++sho payload

       -- table <- readHotVar remoteIvarTable
       table <- modifyHotVar remoteIvarTable (\tbl -> (IntMap.delete ivarid tbl, tbl))

       case IntMap.lookup ivarid table of 
	 Nothing  -> errorExit$ "Processing WorkFinished message, failed to find ivarID in table: "++show ivarid
	 Just parFn -> 
--	   R.pushL longQueue (LongWork { stealver = Nothing, 
-- NEW_REQUE_STRATEGY: Everything nonstealable goes on the right end:
	   R.pushR longQueue (LongWork { stealver = Nothing, 
					 localver = parFn payload
				       })
       rcvLoop myid

     -- This case EXITS the receive loop peacefully:
     ShutDown token -> do
       (num,outstanding) <- measureIVarTable
       dbgTaggedMsg 1 $ "[rcvdmn] -== RECEIVED SHUTDOWN MESSAGE ==-" +++ outstanding
       unless (num == 0) $ errorExit " The number of outstanding longSpawned IVars was NOT zero at shutdown"

       master    <- readHotVar isMaster
       schedMap' <- readHotVar schedMap
       if master then masterShutdown token targetEnd
		 else workerShutdown schedMap'

     other -> error$ "ERROR: ReceiveDaemon received unexpected message: "++ show other



--------------------------------------------------------------------------------
-- <boilerplate>

-- In Debug mode we require that IVar contents be Show-able:
#ifdef DEBUG
longSpawn  :: (Show a, NFData a, Serializable a) 
           => ParClosure a -> Par (IVar a)
#else
longSpawn  :: (NFData a, Serializable a) 
           => ParClosure a -> Par (IVar a)

#endif


#if 0 
-- Option 1: Use the GHC Generics mechanism to derive these:
-- (default methods would make this easier)
instance Serialize Message where
  put = derivePut 
  get = deriveGet
magic_word :: Int64
magic_word = 98989898989898989
-- Note, these encodings of sums are not efficient!
#else 

type TagTy = Word8

-- Even though this is tedious, because I was running into (toEnum)
-- exceptions with the above I'm going to write this out longhand
-- [2012.02.17]:
instance Serialize Message where
 put AnnounceSlave{name,toAddr} = do Ser.put (1::TagTy)
				     Ser.put name
				     Ser.put toAddr
 put (MachineListMsg (n1,bs) n2 ml) = 
                                  do 
                                     Ser.put (2::TagTy)
				     Ser.put n1
				     Ser.put bs
				     Ser.put n2
				     Ser.put ml
 put ConnectedAllPeers =    Ser.put (3::TagTy)
 put StartStealing     =    Ser.put (4::TagTy)
 put (ShutDown tok)    = do Ser.put (5::TagTy)
			    Ser.put tok
 put ShutDownACK       =    Ser.put (6::TagTy)
 put (StealRequest id) = do Ser.put (7::TagTy)
			    Ser.put id
 put (StealResponse id (iv,pay)) = 
                         do Ser.put (8::TagTy)
			    Ser.put id
			    Ser.put iv
			    Ser.put pay
 put (WorkFinished nd iv pay) = 
                         do Ser.put (9::TagTy)
			    Ser.put nd
			    Ser.put iv
			    Ser.put pay
 get = do tag <- Ser.get 
          case tag :: TagTy of 
	    1 -> do name   <- Ser.get 
		    toAddr <- Ser.get
		    return (AnnounceSlave{name,toAddr})
            2 -> do n1 <- Ser.get 
		    bs <- Ser.get 
		    n2 <- Ser.get 
		    ml <- Ser.get 
		    return (MachineListMsg (n1,bs) n2 ml) 
	    3 -> return ConnectedAllPeers
	    4 -> return StartStealing
	    5 -> ShutDown <$> Ser.get
	    6 -> return ShutDownACK
	    7 -> StealRequest <$> Ser.get
	    8 -> do id <- Ser.get
		    iv <- Ser.get
		    pay <- Ser.get
		    return (StealResponse id (iv,pay))
	    9 -> do nd <- Ser.get
		    iv <- Ser.get
		    pay <- Ser.get
		    return (WorkFinished nd iv pay)
            _ -> errorExitPure$ "Remote.hs: Corrupt message: tag header = "++show tag
#endif      
