{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns, DeriveGeneric, ScopedTypeVariables, DeriveDataTypeable #-}

{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- | Resource for Remote execution.

module Control.Monad.Par.Meta.Resources.Remote 
  ( initAction, stealAction, 
    initiateShutdown, waitForShutdown, 
    longSpawn, 
    taggedMsg, InitMode(..)  
  )  
 where

import Control.Applicative    ((<$>))
import Control.Concurrent     (myThreadId, threadDelay, throwTo, killThread,
			       writeChan, readChan, newChan, Chan,
			       forkOS, threadCapability, ThreadId)
import Control.DeepSeq        (NFData)
import Control.Exception      (ErrorCall(..), catch, SomeException)
import Control.Monad          (forM_, when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par.Meta.HotVar.IORef
import Data.Unique            (hashUnique, newUnique)
import Data.Typeable          (Typeable)
import Data.IORef             (writeIORef, readIORef, newIORef, IORef)
import qualified Data.IntMap as IntMap
import qualified Data.Map    as M
import Data.Int               (Int64)
import Data.Maybe             (fromMaybe)
import Data.Char              (isSpace)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Concurrent.Deque.Reference as R
import Data.Concurrent.Deque.Class     as DQ
import Data.List as L
import Data.List.Split    (splitOn)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Set    as Set
import Data.Binary        (encode,decode, Binary)
-- import Data.Binary.Derive (deriveGet, derivePut)
import qualified Data.Binary as Bin
import GHC.Generics       (Generic)
import Prelude     hiding (catch)
import qualified Prelude as P
import System.IO          (hPutStr, hPutStrLn, hFlush, stdout, stderr)
import System.IO.Unsafe   (unsafePerformIO)
import System.Process     (readProcess)
import System.Posix.Process (getProcessID)
import System.Directory   (removeFile, doesFileExist, renameFile)
import System.Random      (randomIO)
import Text.Printf        (printf)

import Control.Monad.Par.Meta hiding (dbg, stealAction)
import qualified Network.Transport     as T
import qualified Network.Transport.TCP as TCP
import Remote2.Closure  (Closure(Closure))
import Remote2.Encoding (Payload, Serializable, serialDecodePure, getPayloadContent, getPayloadType)
import qualified Remote2.Reg as Reg

----------------------------------------------------------------------------------------------------
--                                              TODO                                              --
----------------------------------------------------------------------------------------------------

--   * Make ivarid actually unique.  Per-thread incremented counters would be fine.
--   * Add configurability for constants below.
--   * Get rid of myTransport -- just use the peerTable

-----------------------------------------------------------------------------------
-- Global constants:
-----------------------------------------------------------------------------------

-- This controls how much output will be printed, 0-5.
-- 5 is "debug" mode and affects other aspects of execution (see dbgDelay)
verbosity :: Int
#ifdef DEBUG
verbosity = 5
#else
-- This should be 1 by default but temporarily upping to 2:
verbosity = 2
#endif

dbg :: Bool
dbg = (verbosity>=5)

-- | Machine-unique identifier for 'IVar's
type IVarId = Int

type ParClosure a = (Par a, Closure (Par a))

-- TODO: Make this configurable:
control_port :: String
control_port = "8098"

work_base_port :: Int
work_base_port = 8099

master_addr_file :: String
master_addr_file = "master_control.addr"

mkAddrFile machine n = "./"++show n++"_"++ BS.unpack machine  ++ "_source.addr" 

eager_connections = True
-- Other temporary toggles:
-- define SHUTDOWNACK
#define KILL_WORKERS_ON_SHUTDOWN

-- printErr = hPutStrLn stderr
printErr = putStrLn

--------------------------------------------------------------------------------
-- Global Mutable Structures 
-----------------------------------------------------------------------------------

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
longQueue :: DQ.Queue LongWork
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

{-# NOINLINE global_mode #-}
-- Just for debugging, tracking global node as M (master) or S (slave):
global_mode = unsafePerformIO$ newIORef "_M"




type Token = Int64
{-# NOINLINE shutdownChan #-}
shutdownChan :: Chan Token
shutdownChan = unsafePerformIO $ newChan


-----------------------------------------------------------------------------------
-- Misc Helpers:
-----------------------------------------------------------------------------------

-- forkDaemon = forkIO
forkDaemon name action = 
   forkWithExceptions forkOS ("Daemon thread ("++name++")") action
--   forkOS $ catch action
-- 		 (\ (e :: SomeException) -> do
-- 		  hPutStrLn stderr $ "Caught Error in Daemon thread ("++name++"): " ++ show e
-- 		 )

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
--  table <- readHotVar peerTable
--  let (Just (_,target)) = table V.! ndid
--  T.send target [msg]
    (_,conn) <- connectNode ndid
    T.send conn [msg]

-- | Likewise for Receiving
-- receiveFrom :: NodeID -> IO BS.ByteString
-- receiveFrom ndid = do
--   table <- readHotVar peerTable
--   let (Just (_,conn, ??)) = table V.! ndid
--   bss <- T.receive conn
--   return (BS.concat bss)

taggedMsg lvl s = 
   if verbosity >= lvl then 
      do m <- readIORef global_mode
	 printErr$ " [distmeta"++m++"] "++s
   else return ()

-- When debugging it is helpful to slow down certain fast paths to a human scale:
dbgDelay _ = 
  if   dbg
  then threadDelay (200*1000)
  else return ()

-- | We don't want anyone to try to use a file that isn't completely written.
--   Note, this needs further thought for use with NFS...
atomicWriteFile file contents = do 
   rand :: Int64 <- randomIO
   let tmpfile = "./tmpfile_"++show rand
   BS.writeFile tmpfile contents
   renameFile tmpfile file


-- | This assumes a "Payload closure" as produced by makePayloadClosure below.
deClosure :: Closure Payload -> IO (Par Payload)
-- deClosure :: (Typeable a) => Closure a -> IO (Maybe a)
deClosure pclo@(Closure ident payload) = do 
  taggedMsg 4$ "Declosuring : "++ show ident ++ " type "++ show payload
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
--       tid <- readIORef mainThread
--       throwTo tid (ErrorCall$ "ERROR: "++str)
--       error$ "ERROR: "++str
   printErr$ "ERROR: "++str 
   closeAllConnections
   printErr$ "Connections closed, now exiting process."
   exitProcess 1

foreign import ccall "exit" c_exit :: Int -> IO ()

-- | Exit the process successfully.
exitSuccess :: IO a
exitSuccess = do 
   taggedMsg 1$ "  EXITING process with success exit code."
   closeAllConnections
   -- TODO: Close connections.
   taggedMsg 1$ "   (Connections closed, now exiting for real)"
   exitProcess 0

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

   -- Option 2: kill:
   --      mypid <- getProcessID
   --      system$ "kill "++ show mypid

   -- Option 3: FFI call:
   c_exit code

   putStrLn$ "SHOULD NOT SEE this... process should have exited already."
   return (error "Should not see this.")

errorExitPure :: String -> a
errorExitPure str = unsafePerformIO$ errorExit str

-- TODO:
closeAllConnections = do
  pt <- readHotVar peerTable
  V.forM_ pt $ \ entry -> 
    case entry of 
      Nothing -> return ()
      Just (_,targ) -> T.closeSourceEnd targ
  trans <- readIORef myTransport
  T.closeTransport trans

instance Show Payload where
  show payload = "<type: "++ show (getPayloadType payload) ++", bytes: "++ show (getPayloadContent payload) ++ ">"
    
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
initPeerTable ms = do
     writeIORef  machineList ms
     printErr$ "PeerTable:\n"
     forM_ (zip [0..] ms) $ \ (i,m) -> 
        printErr$ "  "++show i++": "++ BS.unpack m
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
waitReadAndConnect ind file = do 
      taggedMsg 2$ "    Establishing connection, reading file: "++file
      transport <- readIORef myTransport
      let 
          loop bool = do
	  b <- doesFileExist file 
	  if b then BS.readFile file
	   else do if bool then 
                      taggedMsg 2$ "  File does not exist yet...."
		    else when (verbosity>=2) $ do 
                      hPutStr stdout "."
                      hFlush stdout
		   threadDelay (10*1000) -- Wait between file system checks.
		   loop False
      bstr <- loop True
      addr <- case T.deserialize transport bstr of 
 	        Nothing -> fail$ " [distmeta] Garbage addr in file: "++ file
		Just a  -> return a
      conn <- T.connect addr
      taggedMsg 2$ "     ... Connected."
      return conn

extendPeerTable :: Int -> (PeerName, T.SourceEnd) -> IO ()
extendPeerTable id entry = 
  modifyHotVar_ peerTable (V.modify (\v -> MV.write v id (Just entry)))

-----------------------------------------------------------------------------------
-- Data Types
-----------------------------------------------------------------------------------

-- | For now it is the master's job to know the machine list:
data InitMode = Master MachineList | Slave
-- | The MachineList may contain duplicates!!
type MachineList = [PeerName] 

type NodeID = Int

showNodeID n = "<"++show n++">"

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


-----------------------------------------------------------------------------------
-- Main scheduler components (init & steal)
-----------------------------------------------------------------------------------

initAction :: [Reg.RemoteCallMetaData] -> InitMode -> InitAction
  -- For now we bake in assumptions about being able to SSH to the machine_list:

initAction metadata (Master machineList) topStealAction schedMap = 
  do 
     taggedMsg 2$ "Initializing master..."

     writeIORef globalRPCMetadata (Reg.registerCalls metadata)
     taggedMsg 3$ "RPC metadata initialized."

     -- Initialize the peerTable now that we know how many nodes there are:
     initPeerTable machineList

     -- Initialize the transport layer:
     host <- BS.pack <$> hostName
     transport <- TCP.mkTransport $ TCP.TCPConfig T.defaultHints (BS.unpack host) control_port
     writeIORef myTransport transport
     myThreadId >>= writeIORef mainThread
        
     let allButMe = if (elem host machineList) 
		    then delete host machineList
		    else error$ "Could not find master domain name "++BS.unpack host++
			        " in machine list:\n  "++ unwords (map BS.unpack machineList)
         myid = nameToID host 0 machineList
     writeIORef myNodeID myid
     writeIORef masterID myid
     writeIORef isMaster True

     taggedMsg 3$ "Master node id established as: "++showNodeID myid

     (sourceAddr, targetEnd) <- T.newConnection transport
     -- Hygiene: Clear files storing slave addresses:
     forM_ (zip [0..] machineList) $ \ (ind,machine) -> do
        let file = mkAddrFile machine ind
        b <- doesFileExist file
        when b $ removeFile file

     -- Write a file with our address enabling slaves to find us:
     atomicWriteFile master_addr_file $ T.serialize sourceAddr
     -- We are also a worker, so we write the file under our own name as well:
     atomicWriteFile (mkAddrFile host myid) $ T.serialize sourceAddr

     -- Connection Listening Loop:
     ----------------------------------------
     -- Every time we, the master, receive a connection on this port
     -- it indicates a new slave starting up.
     -- As the master we eagerly set up connections with everyone:
     let
         slaveConnectLoop 0    alreadyConnected = return ()
	 slaveConnectLoop iter alreadyConnected = do
          strs <- T.receive targetEnd 
          let str = BS.concat strs
              msg = decode str
          case msg of
            AnnounceSlave{name,toAddr} -> do 
	      taggedMsg 3$ "Master: register new slave, name: "++ BS.unpack name
	      taggedMsg 4$ "  Full message received from slave: "++ (BS.unpack str)

	      -- Deserialize each of the sourceEnds received by the slaves:
	      case T.deserialize transport toAddr of
		Nothing         -> fail "Garbage message from slave!"
		Just sourceAddr -> do 
		  srcEnd <- T.connect sourceAddr
		  let (id, alreadyConnected') = updateConnected name alreadyConnected machineList
		  -- Convention: send slaves the machine list, and tell it what its ID is:
		  T.send srcEnd [encode$ MachineListMsg (myid,host) id machineList]

		  taggedMsg 4$ "Sent machine list to slave "++showNodeID id++": "++ unwords (map BS.unpack machineList)

		  -- Write the file to communicate that the machine is
		  -- online and set up a way to contact it:
		  let filename = mkAddrFile name id

		  atomicWriteFile filename toAddr
		  taggedMsg 3$ "  Wrote file to signify slave online: " ++ filename

		  -- Keep track of the connection for future communications:
		  taggedMsg 4$ "  Extending peerTable with node ID "++ show id
		  extendPeerTable id (name, srcEnd)

		  slaveConnectLoop (iter-1) alreadyConnected' 

            othermsg -> errorExit$ "Received unexpected message when expecting AnnounceSlave: "++show othermsg
     ----------------------------------------
     taggedMsg 3$ "  ... waiting for slaves to connect."
     slaveConnectLoop (length allButMe) (M.singleton host 1)
     taggedMsg 2$ "  ... All slaves announced themselves."

     ----------------------------------------
     -- Possibly wait for slaves to get all their peer connections up:
     when eager_connections $ do 
       taggedMsg 2$ "  Waiting for slaves to bring up all N^2 mutual connections..."
       forM_ (zip [0..] machineList) $ \ (ndid,name) -> 
         unless (ndid == myid) $ do 
          msg <- decode <$> BS.concat <$> T.receive targetEnd 
          case msg of 
	     ConnectedAllPeers -> putStrLn$ "  "++ show ndid ++ ", " ++ BS.unpack name ++ ": peers connected."
	     msg -> errorExit$ "Expected ConnectAllPeers message, received: "++ show msg

     taggedMsg 2$ "  All slaves ready!  Launching receiveDaemon..."
     forkDaemon "ReceiveDaemon"$ receiveDaemon targetEnd schedMap
       
     ----------------------------------------
     -- Allow slaves to proceed past the barrier:     
     forM_ (zip [0..] machineList) $ \ (ndid,name) -> 
       unless (ndid == myid) $ 
	 sendTo ndid (encode StartStealing)

     ----------------------------------------
     taggedMsg 3$ "Master initAction returning control to scheduler..."
     return ()
 where 
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
	 else error$ "nameToID: hostname not in [or not included enough times] in machine list: "++BS.unpack bs1

    basename bs = BS.pack$ head$ splitOn "." (BS.unpack bs)


------------------------------------------------------------------------------------------
-- | TODO - FACTOR OUT TCP-transport SPECIFIC CODE:
initAction metadata Slave topStealAction schedMap = 
  do 
     mypid <- getProcessID
     -- Use the PID to break symmetry between multiple slaves on the same machine:
     let port  = work_base_port + fromIntegral mypid
	 port' = if port > 65535 
                 then (port `mod` (65535-8000)) + 8000
		 else port
     taggedMsg 2$ "Init slave: creating connection on port " ++ show port'
     host <- BS.pack <$> commonInit metadata (show port')
     writeIORef global_mode "_S"

     transport <- readIORef myTransport
     (mySourceAddr, myInbound) <- T.newConnection transport

     taggedMsg 3$ "  Source addr created: " ++ BS.unpack (T.serialize mySourceAddr)

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
             taggedMsg 3$ "Found master's address in file: "++ master_addr_file
             bstr <- BS.readFile master_addr_file
	     case T.deserialize transport bstr of 
	       Nothing -> fail$ " [distmeta] Garbage message in master file: "++ master_addr_file
   	       Just masterAddr -> do 
		 toMaster <- T.connect masterAddr
		 -- Convention: connect by sending name and reverse connection:
		 T.send toMaster [encode$ AnnounceSlave host (T.serialize mySourceAddr)]

                 taggedMsg 3$ "Sent name and addr to master "
	         _machines_bss <- T.receive myInbound
                 let MachineListMsg (masterId,masterName) myid machines = decode (BS.concat _machines_bss)
                 taggedMsg 2$ "Received machine list from master: "++ unwords (map BS.unpack machines)
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
	     taggedMsg 2$ "  Proactively connecting to all peers..."
	     myid <- readIORef myNodeID 
	     forM_ (zip [0..] machines) $ \ (ndid,name) -> 
		unless (ndid == myid) $ do 
		  connectNode ndid
 --			taggedMsg$ "    "++show ndid++": "++BS.unpack name ++" connected."
		  return ()
	     taggedMsg 2$ "  Fully connected, notifying master."
	     sendTo masterId (encode ConnectedAllPeers)
         ----------------------------------------
         waitBarrier = do 
	     -- Don't proceed till we get the go-message from the Master:
	     msg <- T.receive myInbound
	     case decode (BS.concat msg) of 
	       StartStealing -> taggedMsg 1$ "Received 'Go' message from master.  BEGIN STEALING."
	       msg           -> errorExit$ "Expecting StartStealing message, received: "++ show msg
              

     -- These are the key steps:
     waitMasterFile
     pr <- doMasterConnection
     when eager_connections (establishALLConections pr)
     waitBarrier

     forkDaemon "ReceiveDaemon"$ receiveDaemon myInbound schedMap
     taggedMsg 3$ "Slave initAction returning control to scheduler..."

     return ()


-- TODO - FACTOR OUT OF MASTER CASE AS WELL:
-- | Common pieces factored out from the master and slave initializations.
commonInit metadata port = do 
  
     writeIORef globalRPCMetadata (Reg.registerCalls metadata)
     taggedMsg 3$ "RPC metadata initialized."

     host <- hostName

     transport <- TCP.mkTransport $ TCP.TCPConfig T.defaultHints host port
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
masterShutdown token targetEnd = do
   mLs  <- readIORef machineList
   myid <- readIORef myNodeID
   ----------------------------------------  
   forM_ (zip [0..] mLs) $ \ (ndid,name) -> 
     unless (ndid == myid) $ do 
      sendTo ndid (encode$ ShutDown token)
   ----------------------------------------  
#ifdef SHUTDOWNACK
   taggedMsg 1$ "Waiting for ACKs that all slaves shut down..."
   let loop 0 = return ()
       loop n = do msg <- T.receive targetEnd
		   case decode (BS.concat msg) of
		     ShutDownACK -> loop (n-1)
		     other -> do 
                                 taggedMsg 4$ "Warning: received other msg while trying to shutdown.  Might be ok: "++show other
			         loop n
   loop (length mLs - 1)
   taggedMsg 1$ "All nodes shut down successfully."
#endif
   writeChan shutdownChan token

-- TODO: Timeout.


workerShutdown :: (IntMap.IntMap Sched) -> IO ()
workerShutdown schedMap = do
   taggedMsg 1$ "Shutdown initiated for worker."
#ifdef SHUTDOWNACK
   mid <- readIORef masterID
   sendTo mid (encode ShutDownACK)
#endif
   -- Because we are completely shutting down the process we are at
   -- liberty to kill all worker threads here:
#ifdef KILL_WORKERS_ON_SHUTDOWN
   forM_ (IntMap.elems schedMap) $ \ Sched{tids} -> do
--     set <- readHotVar tids
     set <- modifyHotVar tids (\set -> (Set.empty,set))
     mapM_ killThread (Set.toList set) 
   taggedMsg 1$ "  Killed all Par worker threads."
#endif
   exitSuccess

-- Kill own pid:
-- shutDownSelf


--------------------------------------------------------------------------------

stealAction :: StealAction
stealAction Sched{no} _ = do
  dbgDelay "stealAction"

  -- First try to pop local work:
  x <- R.tryPopR longQueue

  case x of 
    Just (LongWork{localver}) -> do
      taggedMsg 3$ "stealAction: worker number "++show no++" found work in own queue."
      return (Just localver)
    Nothing -> raidPeer
 where 
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
     taggedMsg 3$ ""++ showNodeID myid++" Attempting steal from Node ID "++showNodeID ind

--     (_,conn) <- connectNode ind 
--     T.send conn [encode$ StealRequest myid]
     sendTo ind (encode$ StealRequest myid)

     -- We have nothing to return immediately, but hopefully work will come back later.
     return Nothing



longSpawn (local, clo@(Closure n pld)) = do
  let pclo = fromMaybe (errorExitPure "Could not find Payload closure")
                     $ makePayloadClosure clo
  iv <- new
  liftIO$ do
    taggedMsg 4$ " Serialized closure: " ++ show clo

    (cap, _) <- (threadCapability =<< myThreadId)
    when dbg $ printf " [%d] longSpawn'ing computation...\n" cap

    -- Create a unique identifier for this IVar that is valid for the
    -- rest of the current run:
    ivarid <- hashUnique <$> newUnique -- This is not actually safe.  Hashes may collide.

    let ivarCont payl = case serialDecodePure payl of 
			  Just x  -> put_ iv x
			  Nothing -> errorExitPure$ "Could not decode payload: "++ show payl

    -- Update the table with a Par computation that can write in the result.
    modifyHotVar_ remoteIvarTable (IntMap.insert ivarid ivarCont)

    R.pushR longQueue 
       (LongWork{ stealver= Just (ivarid,pclo),
		  localver= do x <- local; put_ iv x
		})

  return iv


--------------------------------------------------------------------------------

-- | Receive steal requests from other nodes.
receiveDaemon :: T.TargetEnd -> HotVar (IntMap.IntMap Sched) -> IO ()
receiveDaemon targetEnd schedMap = 
  do myid <- readIORef myNodeID
     rcvLoop myid
 where 
  rcvLoop myid = do
   dbgDelay "receiveDaemon"
 --  bss  <- if dbg
   bss  <- if False
	   then catch (T.receive targetEnd)
		      (\ (e::SomeException) -> do
		       printErr$ "Exception while attempting to receive message in receive loop."
		       exitSuccess
		      )
	   else T.receive targetEnd
   taggedMsg 4$ "[rcvdmn] Received "++ show (BS.length (BS.concat bss)) ++" byte message..."

   case decode (BS.concat bss) of 
     StealRequest ndid -> do
       taggedMsg 3$ "[rcvdmn] Received StealRequest from: "++ showNodeID ndid
       p <- R.tryPopL longQueue
       case p of 
	 Just (LongWork{stealver= Just stealme}) -> do 
	   taggedMsg 3$ "[rcvdmn]   Had longwork in stock, responding with StealResponse..."
	   sendTo ndid (encode$ StealResponse myid stealme)
 -- TODO: FIXME: Dig deeper into the queue to look for something stealable:
	 Just x -> R.pushL longQueue x
	 Nothing -> return ()
       rcvLoop myid

     StealResponse fromNd pr@(ivarid,pclo) -> do
       taggedMsg 3$ "[rcvdmn] Received Steal RESPONSE from "++showNodeID fromNd++" "++ show pr     

       loc <- deClosure pclo
       R.pushL longQueue (LongWork { stealver = Nothing,
				     localver = do 
						   liftIO$ putStrLn "[rcvdmn] RUNNING STOLEN PAR WORK "
						   payl <- loc
						   liftIO$ putStrLn "[rcvdmn]   DONE running stolen par work."
						   liftIO$ sendTo fromNd (encode$ WorkFinished myid ivarid payl)
						   return ()
				   })
       rcvLoop myid

     WorkFinished fromNd ivarid payload -> do
       taggedMsg 2$ "[rcvdmn] Received WorkFinished from "++showNodeID fromNd++
		    " ivarID "++ show ivarid++" payload: "++show payload
       table <- readHotVar remoteIvarTable
       case IntMap.lookup ivarid table of 
	 Nothing  -> errorExit$ "Processing WorkFinished message, failed to find ivarID in table: "++show ivarid
	 Just parFn -> 
	   R.pushL longQueue (LongWork { stealver = Nothing, 
					 localver = parFn payload
				       })
       rcvLoop myid

     -- This case EXITS the receive loop peacefully:
     ShutDown token -> do
       taggedMsg 1$ "-== RECEIVED SHUTDOWN MESSAGE ==-"
       master    <- readHotVar isMaster
       schedMap' <- readHotVar schedMap
       if master then masterShutdown token targetEnd
		 else workerShutdown schedMap'

     msg -> errorExit$ "Received unexpected message while in receiveDaemon: "++ show msg




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
-- Use the GHC Generics mechanism to derive these:
-- (default methods would make this easier)
instance Binary Message where
  put = derivePut 
  get = deriveGet
#else 

-- Even though this is tedious, because I was running into (toEnum)
-- exceptions with the above I'm going to write this out longhand
-- [2012.02.17]:
type TagTy = Bin.Word8
instance Binary Message where
 put AnnounceSlave{name,toAddr} = do Bin.put (1::TagTy)
				     Bin.put name
				     Bin.put toAddr
 put (MachineListMsg (n1,bs) n2 ml) = 
                                  do 
                                     Bin.put (2::TagTy)
				     Bin.put n1
				     Bin.put bs
				     Bin.put n2
				     Bin.put ml
 put ConnectedAllPeers =    Bin.put (3::TagTy)
 put StartStealing     =    Bin.put (4::TagTy)
 put (ShutDown tok)    = do Bin.put (5::TagTy)
			    Bin.put tok
 put ShutDownACK       =    Bin.put (6::TagTy)
 put (StealRequest id) = do Bin.put (7::TagTy)
			    Bin.put id
 put (StealResponse id (iv,pay)) = 
                         do Bin.put (8::TagTy)
			    Bin.put id
			    Bin.put iv
			    Bin.put pay
 put (WorkFinished nd iv pay) = 
                         do Bin.put (9::TagTy)
			    Bin.put nd
			    Bin.put iv
			    Bin.put pay
 get = do tag <- Bin.get 
          case tag :: TagTy of 
	    1 -> do name   <- Bin.get 
		    toAddr <- Bin.get
		    return (AnnounceSlave{name,toAddr})
            2 -> do n1 <- Bin.get 
		    bs <- Bin.get 
		    n2 <- Bin.get 
		    ml <- Bin.get 
		    return (MachineListMsg (n1,bs) n2 ml) 
	    3 -> return ConnectedAllPeers
	    4 -> return StartStealing
	    5 -> ShutDown <$> Bin.get
	    6 -> return ShutDownACK
	    7 -> StealRequest <$> Bin.get
	    8 -> do id <- Bin.get
		    iv <- Bin.get
		    pay <- Bin.get
		    return (StealResponse id (iv,pay))
	    9 -> do nd <- Bin.get
		    iv <- Bin.get
		    pay <- Bin.get
		    return (WorkFinished nd iv pay)
            _ -> errorExitPure$ "Corrupt message: tag header = "++show tag
#endif      

     
magic_word :: Int64
magic_word = 98989898989898989
-- Note, these encodings of sums are not efficient!
