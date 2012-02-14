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
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Par.Meta.HotVar.IORef

-- import Remote (ProcessM, Payload, MatchM, ProcessId, Serializable, RemoteCallMetaData,
-- 	       matchIf, send, match, receiveTimeout, receiveWait, matchUnknownThrow, 
-- 	       findPeerByRole, getPeers, spawnLocal, nameQueryOrStart, remoteInit)

import Data.Unique (hashUnique, newUnique)
import Data.Typeable (Typeable)
import Data.IORef
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
import Remote2.Encoding (Payload, Serializable, serialDecode)
-- import qualified Remote.Process as P 
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import System.Directory (removeFile, doesFileExist)

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
control_port = "8099"
work_port    = "8098"
master_addr_file = mkAddrFile "master_control"

mkAddrFile machine = "./" ++ machine  ++ "_source.addr" 

--------------------------------------------------------------------------------
-- Global Mutable Structures 

-- An item of work that can be executed either locally, or stolen
-- through the network.
data LongWork = LongWork {
     localver  :: Par (),
     stealver  :: (IVarId, Closure Payload)
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
type PeerName = String
peerTable :: HotVar (V.Vector (PeerName, Maybe T.SourceEnd))
-- peerTable :: HotVar (V.Vector (Maybe (PeerName, T.SourceEnd)))
-- peerTable = unsafePerformIO $ newHotVar V.empty
peerTable = unsafePerformIO $ newHotVar (error "global peerTable uninitialized")

{-# NOINLINE myNodeID #-}
myNodeID :: HotVar Int
myNodeID = unsafePerformIO$ newIORef (error "uninitialized global 'myid'")

-- {-# NOINLINE resultQueue #-}
-- | Result queue is pushed to by the GPU daemon, and popped by the
-- 'Par' workers, meaning the 'WSDeque' is appropriate.
-- resultQueue :: WSDeque (Par ())
-- resultQueue = unsafePerformIO R.newQ

-- --------------------------------------------------------------------------------
-- Misc Helpers:

nameToID :: BS.ByteString -> [BS.ByteString] -> Int
nameToID bs1 bsls = 
  case findIndex (\ bs2 -> bs1 == bs2 
	           || basename bs1 == basename bs2) bsls
  of Nothing -> error$ "nameToID: host not included in machine list: "++BS.unpack bs1
     Just x  -> x
 where 
  basename bs = BS.pack$ head$ splitOn "." (BS.unpack bs)

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
  let (_,Just target) = table V.! ndid
  T.send target [msg]

-- Just for debugging, tracking global node as M (master) or S (slave):
global_mode = unsafePerformIO$ newIORef "M"
taggedMsg s = do m <- readIORef global_mode
		 putStrLn$ " [distmeta"++m++"] "++s

-- When debugging it is helpful to slow down certain fast paths to a human scale:
dbgDelay msg = threadDelay (200*1000)

initPeerTable machineList = 
     writeHotVar peerTable (V.replicate (length machineList) (error "Peer table entry uninitialized"))

-- --------------------------------------------------------------------------------
-- Initialize & Establish workers.
--
-- This initialization and discovery phase could be factored into a
-- separate module.  At the end of the discovery process the global
-- structure is initialized with a table of connections to all other
-- peer nodes.

-- (However, these connections may or may not actually be connected.
-- Lazy connection is permissable.)




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
type MachineList = [BS.ByteString]

type NodeID = Int

-- Control messages are for starting the system up and communicating
-- between slaves and the master.
data ControlMessage = 
     AnnounceSlave { name   :: BS.ByteString, 
		     toAddr :: BS.ByteString }
   | MachineListMsg MachineList
  deriving (Show, Generic)

-- Work messages are for getting things done (stealing work, returning results).
data WorkMessage = 
     -- | A message signifying a steal request and including the NodeID of
     --   the sender:
     StealRequest NodeID
   | StealResponse (IVarId, Closure Payload)
  deriving (Show, Generic, Typeable)

-- Use the GHC Generics mechanism to derive these:
-- (default methods would make this easier)
instance Binary WorkMessage where
  put = derivePut 
  get = deriveGet
instance Binary ControlMessage where
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

initAction :: InitMode -> InitAction
  -- For now we bake in assumptions about being able to SSH to the machine_list:

initAction (Master machineList) schedMap = 
  do 
     taggedMsg$ "Initializing master..."

     -- Initialize the peerTable now that we know how many nodes there are:
     initPeerTable machineList

     -- Initialize the transport layer:
     host <- hostName
     transport <- TCP.mkTransport $ TCP.TCPConfig T.defaultHints host control_port

     (sourceAddr, targetEnd) <- T.newConnection transport
     -- Clear files storing slave addresses:
     forM_ machineList $ \ machine -> do
        let file = mkAddrFile (BS.unpack machine)
        b <- doesFileExist file
        when b $ removeFile file

     -- Write a file with our address enabling slaves to find us:
     BS.writeFile master_addr_file $ T.serialize sourceAddr

     -- Connection Listening Loop:
     ----------------------------------------
     -- Every time we, the master, receive a connection on this port
     -- it indicates a new slave starting up.
     -- As the master we eagerly set up connections with everyone:
     let slaveConnectLoop 0 = return ()
	 slaveConnectLoop iter = do
	  [str] <- T.receive targetEnd
          let AnnounceSlave{name,toAddr} = decode str
	  taggedMsg$ "Master: register new slave, name: "++ BS.unpack name
          taggedMsg$ "  Full message received from slave: "++ (BS.unpack str)

          -- Deserialize each of the sourceEnds received by the slaves:
	  case T.deserialize transport toAddr of
	    Nothing         -> fail "Garbage message from slave!"
	    Just sourceAddr -> do 
              srcEnd <- T.connect sourceAddr
              -- Convention: send slaves the machine list:
              T.send srcEnd machineList

              taggedMsg$ "Sent machine list to slave: "++ unwords (map BS.unpack machineList)

              -- Write the file to communicate that the machine is
              -- online and set up a way to contact it:
              let filename = mkAddrFile (BS.unpack name)
		  id = nameToID name machineList
              BS.writeFile filename toAddr
	      taggedMsg$ "  Wrote file to signify slave online: " ++ filename

              -- Keep track of the connection for future communications:
	      let entry = (BS.unpack name, Just srcEnd)
--              modifyHotVar_ peerTable (\ pt -> V.cons entry pt)
	      taggedMsg$ "  Extending peerTable with node ID "++ show id
              modifyHotVar_ peerTable (\ pt -> V.modify (\v -> MV.write v id entry) pt)

	  slaveConnectLoop (iter-1)
     ----------------------------------------
--     forkIO slaveConnectLoop
--     slaveConnectLoop (-1)  -- Loop forever
     taggedMsg$ "  ... waiting for slaves to connect."
     slaveConnectLoop (length machineList)
     taggedMsg$ "  All slaves connected!  Launching receiveDaemon..."
     forkIO$ receiveDaemon targetEnd

     taggedMsg$ "Master initAction returning control to scheduler..."
     return ()


initAction Slave schedMap = 
  do 
     name <- hostName 
     writeIORef global_mode "S"

     let namebs = BS.pack name
     transport <- TCP.mkTransport $ TCP.TCPConfig T.defaultHints name work_port
     (mySourceAddr, fromMaster) <- T.newConnection transport

     taggedMsg$ "Init slave, source addr = " ++ BS.unpack (T.serialize mySourceAddr)

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
		 T.send toMaster [encode$ AnnounceSlave namebs (T.serialize mySourceAddr)]

                 taggedMsg$ "Sent name and addr to master "
	         machines <- T.receive fromMaster
                 taggedMsg$ "Received machine list from master: "++ unwords (map BS.unpack machines)
		 initPeerTable machines
	         
	         let id = nameToID namebs machines
		 writeIORef myNodeID id
		 return ()

	     return ()
            else do threadDelay (10*1000)
		    masterloop 
     masterloop

     forkIO$ receiveDaemon fromMaster
     taggedMsg$ "Slave initAction returning control to scheduler..."

--     taggedMsg$ "Slave initAction jumping directly into receiveDaemon loop..."
--     receiveDaemon fromMaster
     return ()



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
  -- First try a "provably good steal":
  x <- R.tryPopR longQueue
  case x of 
    Just (LongWork{localver}) -> do
      taggedMsg$ "stealAction: worker number "++show no++" found work in own queue."
      return (Just localver)
    Nothing -> raidPeer
 where 
  raidPeer = do
     n :: Int <- randomIO 
     pt <- readHotVar peerTable
     let ind = n `mod` V.length pt

     myid <- readHotVar myNodeID

     taggedMsg$ "Attempting steal from Node ID "++show ind
     let (_,Just conn) = pt V.! ind
	 msg = StealRequest myid
  -- stealAction _ _ = R.tryPopR resultQueue
     T.send conn [encode msg]

     dbgDelay "stealAction"
     return Nothing

-- TEMP FIXME: INLINING THIS HERE:
makePayloadClosure :: Closure a -> Maybe (Closure Payload)
makePayloadClosure (Closure name arg) = 
                case isSuffixOf "__impl" name of
                  False -> Nothing
                  True -> Just $ Closure (name++"Pl") arg


longSpawn (local, clo@(Closure n pld)) = do
  let pclo = fromMaybe (error "Could not find Payload closure")
                     $ makePayloadClosure clo

  iv <- new
  liftIO$ do
    taggedMsg$ " Serialized closure: " ++ show clo

    (cap, _) <- (threadCapability =<< myThreadId)
    when dbg $ printf " [%d] longSpawn'ing computation...\n" cap

    -- Create a unique identifier for this IVar that is valid for the
    -- rest of the current run:
    ivarid <- hashUnique <$> newUnique -- This is not actually safe.  Hashes may collide.

    R.pushR longQueue 
       (LongWork{ stealver= (ivarid,pclo),
		  localver= do x <- local; put iv x
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

  bss <- T.receive targetEnd
  taggedMsg$ "Received "++ show (BS.length (BS.concat bss)) ++" byte message..."

  case decode (BS.concat bss) of 
    (StealRequest ndid) -> do
      taggedMsg$ "Received StealRequest from: "++ show ndid
      p <- R.tryPopL longQueue
      case p of 
	Nothing -> return ()
	Just (LongWork{stealver}) -> do 
	  taggedMsg$ "  Had longwork to perform, responding with StealResponse..."
	  sendTo ndid (encode$ StealResponse stealver)

    (StealResponse (ivar,pclo)) -> do
      taggedMsg$ "Received Steal RESPONSE "++ show (ivar,pclo)
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

