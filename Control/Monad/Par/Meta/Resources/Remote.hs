{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns, DeriveGeneric, ScopedTypeVariables, DeriveDataTypeable #-}

{-# OPTIONS_GHC -Wall #-}

-- | Resource for Remote execution.

module Control.Monad.Par.Meta.Resources.Remote 
--   (
--     initAction
--   , stealAction
-- --  , spawnAcc
--   )  
 where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.DeepSeq
import Control.Exception.Base (evaluate)
import Control.Exception (throwTo, ErrorCall(..))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Par.Meta.HotVar.IORef

-- import Remote (ProcessM, Payload, MatchM, ProcessId, Serializable, RemoteCallMetaData,
-- 	       matchIf, send, match, receiveTimeout, receiveWait, matchUnknownThrow, 
-- 	       findPeerByRole, getPeers, spawnLocal, nameQueryOrStart, remoteInit)

import Data.Unique (hashUnique, newUnique)
import Data.Typeable (Typeable)
import Data.IORef
import qualified Data.IntMap as IntMap
import qualified Data.Map    as M
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
--import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Concurrent.Deque.Reference as R
import Data.Concurrent.Deque.Class     as DQ
import Data.List as L
import Data.List.Split (splitOn)

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import GHC.Generics (Generic)

-- import Data.Serialize (encode,encodeLazy,decode)
-- import Data.Serialize.Derive
-- Cloud Haskell is in the "binary" rather than "cereal" camp presently:
import Data.Binary (encode,decode, Binary)
import Data.Binary.Derive
import qualified Data.Binary as Bin

-- import qualified Data.Serialize as Bin
-- import Data.Serialize.Put (runPut)

-- import Data.DeriveTH
-- $( derive makeBinary ''WorkMessage )


import Remote2.Closure 
import Remote2.Encoding (Payload, Serializable, serialDecode, serialDecodePure, getPayloadContent, getPayloadType)
-- import qualified Remote2.Reg (RemoteCallMetaData, getEntryByIdent, registerCalls)
import qualified Remote2.Reg as Reg

-- import qualified Remote.Process as P 
import System.IO (hPutStr, hPutStrLn, hFlush, stdout, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import System.Posix.Process (getProcessID)
import System.Directory (removeFile, doesFileExist, renameFile)
import System.Exit (exitFailure)
import System.Random (randomIO)

import Text.Printf

import qualified Network.Transport     as T
import qualified Network.Transport.TCP as TCP


-- import Data.Array.Accelerate
-- #ifdef ACCELERATE_CUDA_BACKEND
-- import qualified Data.Array.Accelerate.CUDA as Acc
-- #else
-- import qualified Data.Array.Accelerate.Interpreter as Acc
-- #endif

-- import Data.Concurrent.Deque.Class (WSDeque)
-- import Data.Concurrent.Deque.Reference as R

-- import System.IO.Unsafe

-- import Text.Printf

import Control.Monad.Par.Meta hiding (dbg, stealAction)

----------------------------------------------------------------------------------------------------
--                                              TODO                                              --
----------------------------------------------------------------------------------------------------

--   * Make ivarid actually unique.  Per-thread incremented counters would be fine.
--   * Add configurability for constants below.

----------------------------------------------------------------------------------------------------

-- Global constants:

dbg :: Bool
#ifdef DEBUG
dbg = True
#else
dbg = False
#endif

-- | Machine-unique identifier for 'IVar's
type IVarId = Int

type ParClosure a = (Par a, Closure (Par a))

-- TODO: Make this configurable:
control_port = "8098"
work_base_port = 8099
master_addr_file = "master_control.addr"

mkAddrFile machine n = "./"++show n++"_"++ BS.unpack machine  ++ "_source.addr" 

--------------------------------------------------------------------------------
-- Global Mutable Structures 

-- An item of work that can be executed either locally, or stolen
-- through the network.
data LongWork = LongWork {
     localver  :: Par (),
     stealver  :: Maybe (IVarId, Closure Payload)
--     writeback :: a -> 
  }

{-# NOINLINE longQueue #-}
-- | Long-stealing queue is pushed to by 'Par' workers, and popped
--   both by par workers and by remote processes.
-- longQueue :: Chan (IO ())
-- longQueue = unsafePerformIO newChan
--longQueue :: HotVar (Deque (IVarId, Closure Payload))
--longQueue :: HotVar (DQ.Queue LongWork)
--longQueue = unsafePerformIO $ R.newQ >>= newHotVar 

longQueue :: DQ.Queue LongWork
longQueue = unsafePerformIO $ R.newQ


{-# NOINLINE peerTable #-}
-- Each peer is either connected, or not connected yet.
type PeerName = BS.ByteString
-- peerTable :: HotVar (V.Vector (PeerName, Maybe T.SourceEnd))
peerTable :: HotVar (V.Vector (Maybe (PeerName, T.SourceEnd)))
-- peerTable = unsafePerformIO $ newHotVar V.empty
peerTable = unsafePerformIO $ newHotVar (error "global peerTable uninitialized")


{-# NOINLINE machineList #-}
machineList :: IORef [BS.ByteString]
machineList = unsafePerformIO$ newIORef (error "global machineList uninitialized")

{-# NOINLINE myTransport #-}
myTransport :: IORef T.Transport
myTransport = unsafePerformIO$ newIORef (error "global myTransport uninitialized")

{-# NOINLINE myNodeID #-}
myNodeID :: HotVar Int
myNodeID = unsafePerformIO$ newIORef (error "uninitialized global 'myid'")

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


-- {-# NOINLINE resultQueue #-}
-- | Result queue is pushed to by the GPU daemon, and popped by the
-- 'Par' workers, meaning the 'WSDeque' is appropriate.
-- resultQueue :: WSDeque (Par ())
-- resultQueue = unsafePerformIO R.newQ

-- --------------------------------------------------------------------------------
-- Misc Helpers:

-- forkDaemon = forkIO
forkDaemon = forkOS

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
  table <- readHotVar peerTable
  let (Just (_,target)) = table V.! ndid
  T.send target [msg]

-- Just for debugging, tracking global node as M (master) or S (slave):
global_mode = unsafePerformIO$ newIORef "_M"
taggedMsg s = 
   if dbg then 
      do m <- readIORef global_mode
	 putStrLn$ " [distmeta"++m++"] "++s
   else return ()

-- When debugging it is helpful to slow down certain fast paths to a human scale:
dbgDelay msg = 
  if dbg then threadDelay (200*1000)
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
  taggedMsg$ "Declosuring : "++ show ident ++ " type "++ show payload
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
      tid <- readIORef mainThread
      throwTo tid (ErrorCall$ "ERROR: "++str)
      error$ "ERROR: "++str

errorExitPure :: String -> a
errorExitPure str = unsafePerformIO$ errorExit str
-- (do errorExit str; return (error "Internal problem: This point should never be reached"))

instance Show Payload where
  show payload = "<type: "++ show (getPayloadType payload) ++", bytes: "++ show (getPayloadContent payload) ++ ">"
    
-- --------------------------------------------------------------------------------
-- Initialize & Establish workers.
--
-- This initialization and discovery phase could be factored into a
-- separate module.  At the end of the discovery process the global
-- structure is initialized with a table of connections to all other
-- peer nodes.

-- (However, these connections may or may not actually be connected.
-- Lazy connection is permissable.)

-- Initialize global state.
initPeerTable ms = do
     writeIORef  machineList ms
     hPutStr stderr "PeerTable:\n"
     forM_ (zip [0..] ms) $ \ (i,m) -> 
        hPutStrLn stderr $ "  "++show i++": "++ BS.unpack m
     writeHotVar peerTable (V.replicate (length ms) Nothing)


-- | Connect to a node if there is not already a connection.  Return
--   the name and active connection.
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
      taggedMsg$ "Establishing connection to node by reading file: "++file
      transport <- readIORef myTransport
--      ml        <- readIORef machineList
      let 
--          name  = ml !! ind
          loop bool = do
	  b <- doesFileExist file 
	  if b then BS.readFile file
	   else do if bool then 
                      taggedMsg$ "  File does not exist yet...."
		    else do 
                      hPutStr stdout "."
                      hFlush stdout
		   threadDelay (10*1000) -- Wait between file system checks.
		   loop False
      bstr <- loop True
      addr <- case T.deserialize transport bstr of 
 	        Nothing -> fail$ " [distmeta] Garbage addr in file: "++ file
		Just a  -> return a
      conn <- T.connect addr
      taggedMsg$ "  ... Connected."
      return conn




-- --------------------------------------------------------------------------------
-- -- spawnAcc operator and init/steal definitions to export

-- spawnAcc :: (Arrays a) => Acc a -> Par (IVar a)
-- spawnAcc comp = do 
--     when dbg $ liftIO $ printf "spawning Accelerate computation\n"
--     iv <- new
--     let wrappedComp = do
--           when dbg $ printf "running Accelerate computation\n"
--           ans <- evaluate $ Acc.run comp
--           R.pushL resultQueue $ do
--             when dbg $ liftIO $ printf "Accelerate computation finished\n"
--             put_ iv ans
--     liftIO $ writeChan gpuQueue wrappedComp
--     return iv               

-- -- | Loop for the GPU daemon; repeatedly takes work off the 'gpuQueue'
-- -- and runs it.
-- gpuDaemon :: IO ()
-- gpuDaemon = do
--   when dbg $ printf "gpu daemon entering loop\n" 
--   join (readChan gpuQueue) >> gpuDaemon

-- | For now it is the master's job to know the machine list:
data InitMode = Master MachineList | Slave
-- | The MachineList may contain duplicates!!
type MachineList = [BS.ByteString] 

type NodeID = Int

showNodeID n = "<"++show n++">"

-- Control messages are for starting the system up and communicating
-- between slaves and the master.
data ControlMessage = 
     AnnounceSlave { name   :: BS.ByteString, 
		     toAddr :: BS.ByteString }
  deriving (Show, Generic)

-- | The master informs the slave of its index in the peerTable:
data MachineListMsg = MachineListMsg Int MachineList
  deriving (Show, Generic)

-- Work messages are for getting things done (stealing work, returning results).
data WorkMessage = 
     -- | A message signifying a steal request and including the NodeID of
     --   the sender:
     StealRequest NodeID
   | StealResponse NodeID (IVarId, Closure Payload)

     -- The result of a parallel computation, evaluated remotely:
   | WorkFinished NodeID IVarId Payload
  deriving (Show, Generic, Typeable)

-- Use the GHC Generics mechanism to derive these:
-- (default methods would make this easier)
instance Binary WorkMessage where
  put = derivePut 
  get = deriveGet
instance Binary ControlMessage where
  put = derivePut 
  get = deriveGet
instance Binary MachineListMsg where
  put = derivePut 
  get = deriveGet
-- Note, these encodings of sums are not efficient!


-- data StealRequest = StealRequest NodeID
--   deriving (Show)

-- data StealResponse = 
--   StealResponse (IVarId, Closure Payload)
-- --  StealResponse (Maybe (IVarId, Closure Payload))
-- --  deriving (Typeable)

-- instance Binary StealRequest where
--   get = do n <- Bin.get
-- 	   return (StealRequest n)
--   put (StealRequest n) = Bin.put n

-- instance Binary StealResponse where
--   get = StealResponse <$> Bin.get 
--   put (StealResponse pld) = Bin.put pld


------------------------------------------------------------------------------------------

initAction :: [Reg.RemoteCallMetaData] -> InitMode -> InitAction
  -- For now we bake in assumptions about being able to SSH to the machine_list:

initAction metadata (Master machineList) topStealAction schedMap = 
  do 
     taggedMsg$ "Initializing master..."

     writeIORef globalRPCMetadata (Reg.registerCalls metadata)
     taggedMsg "RPC metadata initialized."

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
     writeIORef isMaster True

     taggedMsg$ "Master node id established as: "++showNodeID myid

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
	  [str] <- T.receive targetEnd
          let AnnounceSlave{name,toAddr} = decode str
	  taggedMsg$ "Master: register new slave, name: "++ BS.unpack name
          taggedMsg$ "  Full message received from slave: "++ (BS.unpack str)

          -- Deserialize each of the sourceEnds received by the slaves:
	  case T.deserialize transport toAddr of
	    Nothing         -> fail "Garbage message from slave!"
	    Just sourceAddr -> do 
              srcEnd <- T.connect sourceAddr
              let (id, alreadyConnected') = updateConnected name alreadyConnected machineList
              -- Convention: send slaves the machine list:
              T.send srcEnd [encode$ MachineListMsg id machineList]

              taggedMsg$ "Sent machine list to slave "++showNodeID id++": "++ unwords (map BS.unpack machineList)

              -- Write the file to communicate that the machine is
              -- online and set up a way to contact it:
              let filename = mkAddrFile name id

              atomicWriteFile filename toAddr
	      taggedMsg$ "  Wrote file to signify slave online: " ++ filename

              -- Keep track of the connection for future communications:
	      let entry = Just (name, srcEnd)
--              modifyHotVar_ peerTable (\ pt -> V.cons entry pt)
	      taggedMsg$ "  Extending peerTable with node ID "++ show id
              modifyHotVar_ peerTable (\ pt -> V.modify (\v -> MV.write v id entry) pt)

	      slaveConnectLoop (iter-1) alreadyConnected' 
     ----------------------------------------
--     forkIO slaveConnectLoop
--     slaveConnectLoop (-1)  -- Loop forever
     taggedMsg$ "  ... waiting for slaves to connect."
     slaveConnectLoop (length allButMe) (M.singleton host 1)
     taggedMsg$ "  All slaves connected!  Launching receiveDaemon..."
     forkDaemon$ receiveDaemon targetEnd

     taggedMsg$ "Master initAction returning control to scheduler..."
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
    nameToID :: BS.ByteString -> Int -> [BS.ByteString] -> Int
    nameToID bs1 skip bsls = 
      let indices = findIndices (\ bs2 -> bs1 == bs2 
    	 	                -- Hack: handle foo instead of foo.domain.com:
				|| basename bs1 == basename bs2) bsls
      in if length indices > skip 
	 then indices !! skip
	 else error$ "nameToID: hostname not in [or not included enough times] in machine list: "++BS.unpack bs1

    basename bs = BS.pack$ head$ splitOn "." (BS.unpack bs)



-- | TODO - FACTOR OUT TCP-transport SPECIFIC CODE:
initAction metadata Slave topStealAction schedMap = 
  do 
     mypid <- getProcessID
     -- Use the PID to break symmetry between multiple slaves on the same machine:
     let port  = work_base_port + fromIntegral mypid
	 port' = if port > 65535 
                 then (port `mod` (65535-8000)) + 8000
		 else port
     taggedMsg$ "Init slave: creating connection on port " ++ show port'
     host <- BS.pack <$> commonInit metadata (show port')
     writeIORef global_mode "_S"

     transport <- readIORef myTransport
     (mySourceAddr, fromMaster) <- T.newConnection transport

     taggedMsg$ "  Source addr created: " ++ BS.unpack (T.serialize mySourceAddr)

     -- For now: Assume shared filesystem:
     let masterloop = do  -- Loop until connected to master.
           e <- doesFileExist master_addr_file
           if e then do

             taggedMsg$ "Found master's address in file: "++ master_addr_file
             bstr <- BS.readFile master_addr_file
	     case T.deserialize transport bstr of 
	       Nothing -> fail$ " [distmeta] Garbage message in master file: "++ master_addr_file
   	       Just masterAddr -> do 
		 toMaster <- T.connect masterAddr
		 -- Convention: connect by sending name and reverse connection:
		 T.send toMaster [encode$ AnnounceSlave host (T.serialize mySourceAddr)]

                 taggedMsg$ "Sent name and addr to master "
	         _machines_bss <- T.receive fromMaster
                 let MachineListMsg id machines = decode (BS.concat _machines_bss)
                 taggedMsg$ "Received machine list from master: "++ unwords (map BS.unpack machines)
		 initPeerTable machines
	         
		 writeIORef myNodeID id
		 return ()

	     return ()
            else do threadDelay (10*1000) -- Wait between checking filesystem.
		    masterloop 
     masterloop

     forkDaemon$ receiveDaemon fromMaster
     taggedMsg$ "Slave initAction returning control to scheduler..."

--     taggedMsg$ "Slave initAction jumping directly into receiveDaemon loop..."
--     receiveDaemon fromMaster
     return ()


-- TODO - FACTOR OUT OF MASTER CASE AS WELL:
-- | Common pieces factored out from the master and slave initializations.
commonInit metadata port = do 
  
     writeIORef globalRPCMetadata (Reg.registerCalls metadata)
     taggedMsg "RPC metadata initialized."

     host <- hostName

     transport <- TCP.mkTransport $ TCP.TCPConfig T.defaultHints host port
     writeIORef myTransport transport

     -- ASSUME init action is called from main thread:
     myThreadId >>= writeIORef mainThread

     return host




-- TODO: ShutDown function:
{-

     (slaveSourceEnds, masterTargetEnd) <- return (sourceEnds, targetEnd)

     zipWithM_
       (\slaveSourceEnd slaveId -> send slaveSourceEnd [BS.pack . show $ slaveId])
       slaveSourceEnds [0 .. numSlaves-1]
     replicateM_ numSlaves $ do
       [slaveMessage] <- receive masterTargetEnd
       print slaveMessage
	     
     closeTargetEnd masterTargetEnd
     putStrLn "master: close connections"
-}


-- type StealAction =  Sched                 -- ^ 'Sched' for the current thread
--                  -> HotVar (IntMap Sched) -- ^ Map of all 'Sched's
--                  -> IO (Maybe (Par ()))

stealAction :: StealAction
stealAction Sched{no} _ = do
  dbgDelay "stealAction"

  -- First try to pop local work:
  x <- R.tryPopR longQueue

--  master <- readIORef isMaster 

  case x of 
    Just (LongWork{localver}) -> do
      taggedMsg$ "stealAction: worker number "++show no++" found work in own queue."
      return (Just localver)
    Nothing -> -- if master then return Nothing else raidPeer
	       -- TEMP FIXME: Disabling stealing for the Master during DEBUGGING:
	       raidPeer
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
     taggedMsg$ ""++ showNodeID myid++" Attempting steal from Node ID "++showNodeID ind
     (_,conn) <- connectNode ind 
     T.send conn [encode$ StealRequest myid]
     -- We have nothing to return immediately, but hopefully work will come back later.
     return Nothing



longSpawn (local, clo@(Closure n pld)) = do
  let pclo = fromMaybe (errorExitPure "Could not find Payload closure")
                     $ makePayloadClosure clo

  iv <- new
  liftIO$ do
    taggedMsg$ " Serialized closure: " ++ show clo

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



--   liftIO $ do 


--     let pred (WorkFinished iid _)      = iid == ivarid
--         -- the "continuation" to be invoked when receiving a
--         -- 'WorkFinished' message for our 'IVarId'
--         matchThis (WorkFinished _ pld) = liftIO $ do
--           (cap, _) <- threadCapability =<< myThreadId
--           when dbgR $ printf " [%d] Receive answer from longSpawn\n" cap
--           putResult pld
--         putResult pld = do
--           (cap, _) <- threadCapability =<< myThreadId
--           dpld <- fromMaybe (error "failed to decode payload") 
--                         <$> serialDecode pld
--           when dbgR $ printf " [%d] Pushing computation to put remote result into local IVar...\n" cap
--           pushWork cap $ put_ iv dpld
--           modifyHotVar_ matchIVars (IntMap.delete ivarid)
--     modifyHotVar_ matchIVars (IntMap.insert ivarid 
--                                             (matchIf pred matchThis, putResult))
--     when dbgR$ do (no, _) <- threadCapability =<< myThreadId
--                   printf " [%d] Pushing work %s on longQueue\n" no n
--     modifyHotVar_ longQueue (addback (ivarid, pclo))
-- --    when dbg $ do q <- readHotVar longQueue
-- --                  printf " lq: %s\n" (show (dqToList q))
--   return iv


--------------------------------------------------------------------------------

-- | Receive steal requests from other nodes.
receiveDaemon :: T.TargetEnd -> IO ()
receiveDaemon targetEnd = do
  dbgDelay "receiveDaemon"

  myid <- readIORef myNodeID
  bss  <- T.receive targetEnd
  taggedMsg$ "[rcvdmn] Received "++ show (BS.length (BS.concat bss)) ++" byte message..."

  case decode (BS.concat bss) of 
    StealRequest ndid -> do
      taggedMsg$ "[rcvdmn] Received StealRequest from: "++ showNodeID ndid
      p <- R.tryPopL longQueue
      case p of 
	Just (LongWork{stealver= Just stealme}) -> do 
	  taggedMsg$ "[rcvdmn]   Had longwork in stock, responding with StealResponse..."
	  sendTo ndid (encode$ StealResponse myid stealme)
-- TODO: FIXME: Dig deeper into the queue to look for something stealable:
	Just x -> R.pushL longQueue x
        Nothing -> return ()

    StealResponse fromNd pr@(ivarid,pclo) -> do
      taggedMsg$ "[rcvdmn] Received Steal RESPONSE from "++showNodeID fromNd++" "++ show pr     

      loc <- deClosure pclo
      R.pushL longQueue (LongWork { stealver = Nothing,
				    localver = do 
                                                  liftIO$ putStrLn "[rcvdmn] RUNNING STOLEN PAR WORK "
                                                  payl <- loc
                                                  liftIO$ putStrLn "[rcvdmn]   DONE running stolen par work."
				                  liftIO$ sendTo fromNd (encode$ WorkFinished myid ivarid payl)
                                                  return ()
				  })

    WorkFinished fromNd ivarid payload -> do
      taggedMsg$ "[rcvdmn] Received WorkFinished from "++showNodeID fromNd++" ivarID "++ show ivarid++" payload: "++show payload
      table <- readHotVar remoteIvarTable
      case IntMap.lookup ivarid table of 
        Nothing  -> errorExit$ "Processing WorkFinished message, failed to find ivarID in table: "++show ivarid
        Just parFn -> 
          R.pushL longQueue (LongWork { stealver = Nothing, 
			 	        localver = parFn payload
				      })
      return ()

  receiveDaemon targetEnd
 where 

-- receiveDaemon :: ProcessM ()
-- receiveDaemon = do
-- --    when dbg $ liftIO $ do iids <- map fst <$> readHotVar matchIVars
-- --                           printf "IVars %s\n" (show iids)
--     matchIVars <- IntMap.foldr' ((:) . fst) 
--                     [matchPeerList, matchUnknownThrow] -- fallthrough cases
--                     <$> liftIO (readHotVar matchIVars)
--     liftIO$ printf "[PID %s] receiveDaemon: Starting receiveTimeout\n" ospid
--     receiveTimeout 10000 $ (matchSteal:matchIVars)
--     liftIO$ printf "[PID %s] receiveDaemon: receive timed out, looping..\n" ospid
--     receiveDaemon
--   where
--     matchPeerList = match $ \(PeerList pids) -> liftIO $
--                       modifyHotVar_ parWorkerPids (const $ V.fromList pids)
--     matchSteal = P.roundtripResponse $ \StealRequest -> do
--                    when dbgR $ liftIO $ printf "[PID %s] Got StealRequest message!! \n" ospid
--                    p <- liftIO $ modifyHotVar longQueue takefront
--                    case p of
--                      Just _ -> do
--                        when dbgR $ liftIO $ printf "[PID %s] Sending work to remote thief\n" ospid
--                        return (StealResponse p, ())
--                      Nothing -> return (StealResponse Nothing, ())




--------------------------------------------------------------------------------
-- <boilerplate>

-- In Debug mode we require that IVar contents be Show-able:
#ifdef DEBUG
-- put    :: (Show a, NFData a) => IVar a -> a -> Par ()
-- spawn  :: (Show a, NFData a) => Par a -> Par (IVar a)
-- spawn_ :: Show a => Par a -> Par (IVar a)
-- spawnP :: (Show a, NFData a) => a -> Par (IVar a)
-- put_   :: Show a => IVar a -> a -> Par ()
-- get    :: Show a => IVar a -> Par a
-- newFull :: (Show a, NFData a) => a -> Par (IVar a)
-- newFull_ ::  Show a => a -> Par (IVar a)
-- runPar   :: Show a => Par a -> a
-- runParIO :: Show a => Par a -> IO a
longSpawn  :: (Show a, NFData a, Serializable a) 
           => ParClosure a -> Par (IVar a)
#else
-- spawn      :: NFData a => Par a -> Par (IVar a)
-- spawn_     :: Par a -> Par (IVar a)
-- spawnP     :: NFData a => a -> Par (IVar a)
-- put_       :: IVar a -> a -> Par ()
-- put        :: NFData a => IVar a -> a -> Par ()
-- get        :: IVar a -> Par a
-- runPar     :: Par a -> a
-- runParIO   :: Par a -> IO a

-- TODO: Figure out the type signature for this. Should it be a
-- wrapper around CH's remoteInit? How much flexibility should we
-- offer with args?
longSpawn  :: (NFData a, Serializable a) 
           => ParClosure a -> Par (IVar a)
-- newFull    :: NFData a => a -> Par (IVar a)
-- newFull_   :: a -> Par (IVar a)


-- instance PC.ParFuture Par IVar where
--   get    = get
--   spawn  = spawn
--   spawn_ = spawn_
--   spawnP = spawnP

-- instance PC.ParIVar Par IVar where
--   fork = fork
--   new  = new
--   put_ = put_
--   newFull = newFull
--   newFull_ = newFull_

#endif



----------------------------------------------------------------------------------------------------
-- SCRAP


-- This uses a byte per bit:
-- test = runPut$ 
--        do Bin.put True
-- 	  Bin.put False
-- 	  Bin.put True
-- 	  Bin.put False
-- 	  Bin.put True

