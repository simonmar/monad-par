{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}

-- | This module implements a Meta-Par 'Resource' for SMP parallelism,
-- suitable as a base for combined execution resources (e.g.,
-- @Control.Monad.Par.Meta.AccSMP@).
module Control.Monad.Par.Meta.Resources.SMP (
    -- * Resource creation
    mkResource
  , mkResourceOn
    -- * Default implementation
  , defaultStartup
  , defaultWorkSearch
    -- * Customizable implementation
  , startupForCaps
  , wsForCaps
) where

import Control.Monad

import Data.Concurrent.Deque.Reference as R
import Data.List (nub)
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS

import System.Environment (getEnvironment)
import System.IO.Unsafe
import System.Random.MWC

import Text.Printf

import Control.Monad.Par.Meta

import Control.Monad.Par.Meta.HotVar.IORef
import Control.Monad.Par.Meta.Resources.Debugging (dbgTaggedMsg)

#if __GLASGOW_HASKELL__ >= 702
import Control.Concurrent (getNumCapabilities)
#else
import GHC.Conc (numCapabilities)
getNumCapabilities = return numCapabilities
#endif


-- | Create an SMP resource for all capabilities. 
mkResource :: Int -- ^ The number of steal attempts per 'WorkSearch' call.
           -> Resource
mkResource tries = Resource defaultStartup (defaultWorkSearch tries)

-- | Create an SMP resource for a configurable list of capabilities.
mkResourceOn :: [Int] -- ^ Capability list.
             -> Int   -- ^ The number of steal attempts per 'WorkSearch' call.
             -> Resource
mkResourceOn caps tries = Resource (startupForCaps caps) (wsForCaps caps tries)

{-# NOINLINE getCaps #-}
getCaps :: [Int]
getCaps = unsafePerformIO $ do
  env <- getEnvironment
  case lookup "SMP_CAPS" env of
    Just cs -> do
      dbgTaggedMsg 2 $ BS.pack $ printf "[SMP] initialized with capability list %s\n" 
                       (show ((read cs) :: [Int]))
      return $ read cs
    Nothing -> do 
      n <- getNumCapabilities
      dbgTaggedMsg 2 $ BS.pack $ printf "[SMP] initialized with capability list %s\n" 
                   (show ([0..n-1] :: [Int])) 
      return [0..n-1]

-- | 'Startup' for spawning threads on all capabilities, or from a
-- 'read'-able list of capability numbers in the environment variable
-- @SMP_CAPS@.
defaultStartup :: Startup
defaultStartup = startupForCaps getCaps
  
-- | 'Startup' for spawning threads only on a particular set of
-- capabilities.
startupForCaps :: [Int] -> Startup
startupForCaps caps = St st
  where st ws _ = do
          dbgTaggedMsg 2 $ BS.pack $ printf "spawning worker threads for shared memory on caps:\n"
          dbgTaggedMsg 2 $ BS.pack $ printf "\t%s\n" (show caps)
          let caps' = nub caps
          forM_ caps' $ \n ->
            spawnWorkerOnCPU ws n >> return ()

{-# INLINE randModN #-}
randModN :: Int -> HotVar GenIO -> IO Int
randModN caps rngRef = uniformR (0, caps-1) =<< readHotVar rngRef

-- | 'WorkSearch' for all capabilities.
defaultWorkSearch :: Int -> WorkSearch
defaultWorkSearch = wsForCaps getCaps                

-- | Given a set of capabilities and a number of steals to attempt per
-- capability, return a 'WorkSearch'.
wsForCaps :: [Int] -> Int -> WorkSearch
wsForCaps caps triesPerCap = WS ws
  where 
    numCaps = length caps
    numTries = numCaps * triesPerCap
    capVec = V.fromList caps
    ws Sched { no, rng } schedsRef = do
      scheds <- readHotVar schedsRef
      let {-# INLINE getNext #-}
          getNext :: IO Int
          getNext = randModN numCaps rng
          -- | Main steal loop
          loop :: Int -> Int -> IO (Maybe (Par ()))
          loop 0 _ = return Nothing
          loop n i | capVec V.! i == no = loop (n-1) =<< getNext
                   | otherwise =
            let target = capVec V.! i in
            case scheds V.! target of
              Nothing -> do 
                dbgTaggedMsg 2 $ BS.pack $  
                  printf "WARNING: no Sched for cap %d during steal\n" target
                loop (n-1) =<< getNext
              Just Sched { workpool = stealee } -> do
                mtask <- R.tryPopR stealee
                case mtask of
                  Nothing -> loop (n-1) =<< getNext
                  jtask -> return jtask                        
      loop numTries =<< getNext