{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
{-# OPTIONS_GHC -O2 -ddump-splices #-}

-- Simple tests of distributed work stealing.
----------------------------------------------

import System.Environment (getArgs)
import qualified Control.Monad.Par.Meta.Dist as D

import Control.Monad.IO.Class (liftIO)
import Control.Monad (mapM_)
-- Tweaked version of CloudHaskell's closures:
import Remote2.Call (mkClosureRec, remotable)

import Control.Concurrent   (myThreadId, threadDelay)
import System.Process       (readProcess)
import System.Posix.Process (getProcessID)
import Data.Char            (isSpace)
import Data.Typeable
import Data.Binary 

import qualified Data.ByteString.Char8 as BS

-- import qualified Data.IntMap as M
-- import qualified Data.Set as M
--------------------------------------------------------------------------------


-- A description of a fake work topology which is executed by sleeping
-- (threadDelay) different amounts at different points.
-- Presently this describes futures-only work:
data FakeWork = Work Time FakeWork
	      | Fork FakeWork FakeWork
              | SyncAll FakeWork
              | Nop

--	      | Spawn Id FakeWork FakeWork
--              | Seq [FakeWork]
--              | Sync Id FakeWork
 deriving (Eq,Show,Read,Typeable)
-- All constructors take a continuation.              

type Time = Int -- Time in milliseconds
type Id   = Int

-- A simple test that ensures a single steal for two workers:
-- t1 = Spawn 11 (Work 101) $ 
--      Seq [Work 102, 
-- 	  Sync 11]
     
t1 = Fork (Work 101 Nop) $ 
     Work 102 $ 
     SyncAll Nop

t2 = switcher 3 3 True

-- This one alternates where the *real* work will be.
--
-- Takes number of switches.  Performs a shrinking number of work
-- items per "straightaway":
switcher 1 m _     = Work 100 (SyncAll Nop)
switcher _ _ False = Work 101 (SyncAll Nop)
switcher n m True  = Work 102 $ switcher n (m-1) True
switcher n 1 b     = Fork (switcher (n-1) n b) 
		          (switcher (n-1) n (not b))

----------------------------------------------------------------------------------------------------

runTest :: FakeWork -> D.Par ()
--runTest fw = theloop fw M.empty
runTest fw = theloop fw []

theloop fw ls =
   case fw of 
     Nop -> return ()
     Work t cont -> 
        do liftIO$ threadDelay (t * 1000)
	   liftIO$ putStrLn$ "Finished work "++show t
	   theloop cont ls
--     Seq ls -> mapM_ theloop ls
--     Spawn id fw cont -> 
     Fork child cont -> 
        do 
           iv <- D.longSpawn$  $(mkClosureRec 'runTest) child
           -- Associate id with ivar
--	   theloop cont (M.insert id iv mp)
	   theloop cont (iv:ls)
     SyncAll cont -> 
        do mapM_ D.get ls
	   theloop cont []

-- Generate stub code for RPC:
remotable ['runTest]

-- instance Serialize FakeWork where 

instance Binary FakeWork where 
  put fw = put (show fw)
--  get str = return (read (BS.unpack str))
  get = get >>=  return . read


----------------------------------------------------------------------------------------------------

main = do 
    [version] <- getArgs
    case version of 
        "slave" -> D.runParSlaveWithTransport [__remoteCallMetaData] D.TCP
        "master" -> do 	
		       D.runParDistWithTransport [__remoteCallMetaData] D.TCP (runTest t1)
		       putStrLn $ "Finished with work.  Calling SHUTDOWN..."
                       D.shutdownDist
		       putStrLn $ "... returned from shutdown, apparently successful."

        str -> error$"Unhandled mode: " ++ str
