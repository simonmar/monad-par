{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wall #-}

-- | Resource for Remote execution.

module Control.Monad.Par.Meta.Resources.Remote (
    initAction
  , stealAction
--  , spawnAcc
  ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception.Base (evaluate)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Par.Meta.HotVar.IORef

-- import Remote (ProcessM, Payload, MatchM, ProcessId, Serializable, RemoteCallMetaData,
-- 	       matchIf, send, match, receiveTimeout, receiveWait, matchUnknownThrow, 
-- 	       findPeerByRole, getPeers, spawnLocal, nameQueryOrStart, remoteInit)

import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
--import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Concurrent.Deque.Reference as R
import Data.Concurrent.Deque.Class     as DQ

import qualified Data.Vector as V
import Data.Serialize (encode,encodeLazy,decode)

import Remote (Payload, Serializable)
import Remote.Closure
import Remote.Encoding (serialDecode)
import qualified Remote.Process as P 
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import System.Directory (removeFile, doesFileExist)
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

dbg :: Bool
#ifdef DEBUG
dbg = True
#else
dbg = False
#endif

-- | Machine-unique identifier for 'IVar's
type IVarId = Int

-- TODO: Make this configurable:
the_port = "8099"
master_addr_file = "./master_source_addr.txt"

--------------------------------------------------------------------------------
-- Global structures 

-- An item of work that can be executed either locally, or stolen
-- through the network.
data LongWork = LongWork {
     localver  :: IO (),
     stealver  :: (IVarId, Closure Payload)
--     writeback :: a -> 
  }

{-# NOINLINE longQueue #-}
-- | Long-stealing queue is pushed to by 'Par' workers, and popped
--   both by par workers and by remote processes.
-- longQueue :: Chan (IO ())
--longQueue :: HotVar (Deque (IVarId, Closure Payload))
longQueue :: HotVar (DQ.Queue LongWork)
longQueue = unsafePerformIO $ R.newQ >>= newHotVar 
-- longQueue = unsafePerformIO newChan

-- Each peer is either connected, or not connected yet.
type PeerName = String
peerTable :: HotVar (V.Vector (PeerName, Maybe T.SourceEnd))
peerTable = unsafePerformIO $ newHotVar V.empty

-- {-# NOINLINE resultQueue #-}
-- | Result queue is pushed to by the GPU daemon, and popped by the
-- 'Par' workers, meaning the 'WSDeque' is appropriate.
-- resultQueue :: WSDeque (Par ())
-- resultQueue = unsafePerformIO R.newQ

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

data InitMode = Master | Client

data ControlMessage

initAction :: InitMode -> InitAction
  -- For now we bake in assumptions about being able to SSH to the machine_list:

initAction Master schedMap = 
  do forkIO receiveDaemon

     -- Initialize the transport layer:
     let 
	 host    = "localhost" 
	 sourceAddrFilePath = master_addr_file
	 machineList = ["foobar","baz"]
     transport <- TCP.mkTransport $ TCP.TCPConfig T.defaultHints host the_port

     (sourceAddr, targetEnd) <- T.newConnection transport
     -- Clear files storing client addresses:
     forM_ machineList $ \ machine -> 
        removeFile$ machine ++ "_source_addr.txt" 

     -- Write a file with our address enabling clients to find us:
     BS.writeFile sourceAddrFilePath $ T.serialize sourceAddr

     -- Connection Listening Loop:
     ----------------------------------------
     -- Every time we, the master, receive a connection on this port
     -- it indicates a new client starting up.
     -- As the master we eagerly set up connections with everyone:
     let clientConnectLoop = do
	  [name,srcAddrBytes] <- T.receive targetEnd
	  putStrLn$ "master node: register new client: "++ BS.unpack name
          -- Deserialize each of the sourceEnds received by the clients:
	  case T.deserialize transport srcAddrBytes of
	    Nothing         -> fail "Garbage message from client!"
	    Just sourceAddr -> do 
              srcEnd <- T.connect sourceAddr
              -- Convention: send clients the machine list:
              T.send srcEnd (map BS.pack machineList)
              -- Write the file to communicate that the machine is
              -- online and set up a way to contact it:
              let filename = BS.unpack name ++ ".txt"
              BS.writeFile filename srcAddrBytes
	      putStrLn$ "Wrote file to signify client online: " ++ filename

              -- Keep track of the connection for future communications:
              modifyHotVar_ peerTable (\ pt -> V.cons (BS.unpack name, Just srcEnd) pt)	      

	  clientConnectLoop 
     ----------------------------------------
     forkIO clientConnectLoop
     return ()


initAction Client schedMap = 
  do 
--     forkIO receiveDaemon  
     let host = "localhost"
     transport <- TCP.mkTransport $ TCP.TCPConfig T.defaultHints host the_port
     (mySourceAddr, targetEnd) <- T.newConnection transport

     -- For now: Assume shared filesystem:
     let masterloop = do
           e <- doesFileExist master_addr_file
           if e then do
             bstr <- BS.readFile master_addr_file
	     case T.deserialize transport bstr of 
	       Nothing -> fail$ "Garbage message in master file: "++ master_addr_file
   	       Just masterAddr -> do 
		 toMaster <- T.connect masterAddr
                 name <- hostName 
		 -- Convention: connect by sending name and reverse connection:
		 T.send toMaster [BS.pack name, T.serialize mySourceAddr]

		 -- Write the file to communicate that the machine is online:
-- 		 let filename = BS.unpack name ++ ".txt"
-- 		 writeFile filename "I'm online."
-- 		 putStrLn$ "Wrote file to signify client online: " ++ filename

-- 		 -- Keep track of the connection for future communications:
-- 		 modifyHotVar_ peerTable (\ pt -> V.cons (BS.unpack name, Just srcEnd) pt)	      
	         
	     return ()
            else masterloop 

     return ()

hostName = liftM trim $
    readProcess "uname" ["-n"] ""
 where 
  -- | Trim whitespace from both ends of a string.
  trim :: String -> String
  trim = f . f
     where f = reverse . dropWhile isSpace


-- TODO: ShutDown function:
{-

     (clientSourceEnds, masterTargetEnd) <- return (sourceEnds, targetEnd)

     zipWithM_
       (\clientSourceEnd clientId -> send clientSourceEnd [BS.pack . show $ clientId])
       clientSourceEnds [0 .. numSlaves-1]
     replicateM_ numSlaves $ do
       [clientMessage] <- receive masterTargetEnd
       print clientMessage
	     
     closeTargetEnd masterTargetEnd
     putStrLn "master: close connections"
-}


stealAction :: StealAction
stealAction = undefined
-- stealAction _ _ = R.tryPopR resultQueue


longSpawn clo@(Closure n pld) = do
  let pclo = fromMaybe (error "Could not find Payload closure")
                     $ P.makePayloadClosure clo
  liftIO$ do
    (cap, _) <- (threadCapability =<< myThreadId)
    when dbg $ printf " [%d] longSpawn'ing computation...\n" cap

--  modifyHotVar_ longQueue (addback (ivarid, pclo))

  return undefined


--   iv <- new
--   liftIO $ do 
--     -- Create a unique identifier for this IVar that is valid for the
--     -- rest of the current run:
--     ivarid <- hashUnique <$> newUnique
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
receiveDaemon :: IO ()
receiveDaemon = do

  receiveDaemon
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
           => Closure (Par a) -> Par (IVar a)
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
           => Closure (Par a) -> Par (IVar a)
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

