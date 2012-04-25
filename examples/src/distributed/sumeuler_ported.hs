-- Sum of totients in HdpH
--
-- Visibility: HdpH test suite
-- Author: Patrick Maier <P.Maier@hw.ac.uk>
-- Created: 17 Jul 2011
--
-- Ported to meta-par by Ryan Newton [2012.03.09]
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}  -- req'd for some DecodeStatic instances

module Main where

import Prelude
import Control.Exception (evaluate)
import Control.Monad (when, (<=<))
import Data.List (elemIndex, stripPrefix)
import Data.Functor ((<$>))
import Data.List (transpose)
import Data.Maybe (fromJust)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Monoid (mconcat)
import System.Environment (getArgs)
import System.IO (stdout, stderr, hSetBuffering, BufferMode(..))
import System.Random (mkStdGen, setStdGen)

-- import qualified MP.MPI_ByteString as MPI
-- import HdpH (RTSConf(..), defaultRTSConf,
--              Par, runParIO,
--              force, fork, spark, new, get, put, glob, rput,
--              IVar,
--              Env, encodeEnv, decodeEnv,
--              Closure, toClosure, unsafeMkClosure, unsafeMkClosure0, unClosure,
--              DecodeStatic, decodeStatic,
--              Static, staticAs, declare, register)
-- import HdpH.Strategies (Strategy,
--                         ForceClosureStatic, forceClosureStatic,
--                         parMapNF, parMapChunkedNF, parMapSlicedNF)
-- import qualified HdpH.Strategies as Strategies (registerStatic)
-- import HdpH.Internal.Misc (timeIO)  -- for measuring runtime

import Control.Monad.Par.Meta.DistSMP
        (longSpawn, Par, get, shutdownDist, WhichTransport(Pipes,TCP),
	 runParDistWithTransport, runParSlaveWithTransport, 
	 spawn_, put, IVar, new, fork)
-- Tweaked version of CloudHaskell's closures:
import RPC.Call (mkClosureRec, remotable)
import DistDefaultMain

-----------------------------------------------------------------------------
-- 'Static' declaration and registration

-- instance DecodeStatic Int
-- instance DecodeStatic [Int]
-- instance DecodeStatic Integer
-- instance ForceClosureStatic Integer

-- registerStatic :: IO ()
-- registerStatic = do
--   Strategies.registerStatic  -- reg Static terms in imported module
--   register $ mconcat
--     [declare totient_Static,
--      declare sum_totient_Static,
--      declare spark_sum_euler_Static,
--      declare (decodeStatic :: Static (Env -> Int)),
--      declare (decodeStatic :: Static (Env -> [Int])),
--      declare (decodeStatic :: Static (Env -> Integer)),
--      declare (forceClosureStatic ::
--                 Static (Env -> Strategy (Closure Integer)))]


-----------------------------------------------------------------------------
-- Euler's totient function (for positive integers)

totient :: Int -> Integer
totient n = toInteger $ length $ filter (\ k -> gcd n k == 1) [1 .. n]

-- totient_Static :: Static (Env -> Int -> Integer)
-- totient_Static = staticAs
--   (const totient)
--   "Main.totient"

-----------------------------------------------------------------------------
-- sequential sum of totients

-- sum_totient :: [Int] -> Integer
-- sum_totient = sum . map totient

sum_totient :: [Int] -> Par Integer
sum_totient ls = return (sum$ map totient ls)


-- sum_totient_Static :: Static (Env -> [Int] -> Integer)
-- sum_totient_Static = staticAs
--   (const sum_totient)
--   "Main.sum_totient"

-- Generate stub code for RPC:
remotable ['totient, 'sum_totient]

-----------------------------------------------------------------------------
-- parallel sum of totients; shared memory

par_sum_totient_chunked :: Int -> Int -> Int -> Par Integer
par_sum_totient_chunked lower upper chunksize =
  sum <$> (mapM get =<< (mapM fork_sum_euler $ chunked_list))
    where
      chunked_list = chunk chunksize [upper, upper - 1 .. lower] :: [[Int]]


par_sum_totient_sliced :: Int -> Int -> Int -> Par Integer
par_sum_totient_sliced lower upper slices =
  sum <$> (mapM get =<< (mapM fork_sum_euler $ sliced_list))
    where
      sliced_list = slice slices [upper, upper - 1 .. lower] :: [[Int]]


fork_sum_euler :: [Int] -> Par (IVar Integer)
fork_sum_euler xs = do 
                       -- v <- new
--                       fork $ force (sum_totient xs) >>= put v

--                       v <- spawn_ (return$ sum_totient xs) 
                       v <- spawn_ (sum_totient xs) 
                       return v

-----------------------------------------------------------------------------
-- parallel sum of totients; distributed memory

dist_sum_totient_chunked :: Int -> Int -> Int -> Par Integer
dist_sum_totient_chunked lower upper chunksize = do
--  sum <$> (mapM get_and_unClosure =<< (mapM spark_sum_euler $ chunked_list))
  sum <$> (mapM get =<< (mapM spark_sum_euler $ chunked_list))
    where
      chunked_list = chunk chunksize [upper, upper - 1 .. lower] :: [[Int]]


dist_sum_totient_sliced :: Int -> Int -> Int -> Par Integer
dist_sum_totient_sliced lower upper slices = do
--  sum <$> (mapM get_and_unClosure =<< (mapM spark_sum_euler $ sliced_list))
  sum <$> (mapM get =<< (mapM spark_sum_euler $ sliced_list))
    where
      sliced_list = slice slices [upper, upper - 1 .. lower] :: [[Int]]


-- spark_sum_euler :: [Int] -> Par (IVar (Closure Integer))
spark_sum_euler :: [Int] -> Par (IVar Integer)
spark_sum_euler xs = do 
-- Original, HDpH version:
--------------------------
--  v <- new
--  gv <- glob v
--  let val = force (sum_totient xs) >>= rput gv . toClosure
--  let env = encodeEnv (xs, gv)
--  let fun = spark_sum_euler_Static
--  spark $ unsafeMkClosure val fun env
--  return v
--------------------------
-- Meta-par version:
  longSpawn$ $(mkClosureRec 'sum_totient) xs


-- spark_sum_euler_Static :: Static (Env -> Par ())
-- spark_sum_euler_Static = staticAs
--   (\ env -> let (xs, gv) = decodeEnv env
--               in force (sum_totient xs) >>= rput gv . toClosure)
--   "Main.spark_sum_euler"

-- get_and_unClosure :: IVar (Closure a) -> Par a
-- get_and_unClosure = return . unClosure <=< get


-----------------------------------------------------------------------------
-- parallel sum of totients; distributed memory (using plain task farm)

-- farm_sum_totient_chunked :: Int -> Int -> Int -> Par Integer
-- farm_sum_totient_chunked lower upper chunksize =
--   sum <$> parMapNF clo_sum_totient chunked_list
--     where 
--       chunked_list = chunk chunksize [upper, upper - 1 .. lower] :: [[Int]]


-- farm_sum_totient_sliced :: Int -> Int -> Int -> Par Integer
-- farm_sum_totient_sliced lower upper slices =
--   sum <$> parMapNF clo_sum_totient sliced_list
--     where 
--       sliced_list = slice slices [upper, upper - 1 .. lower] :: [[Int]]


-- clo_sum_totient :: Closure ([Int] -> Integer)
-- clo_sum_totient = unsafeMkClosure0 val fun
--   where val = sum_totient
--         fun = sum_totient_Static

{-

-----------------------------------------------------------------------------
-- parallel sum of totients; distributed memory (chunking/slicing task farms)

chunkfarm_sum_totient :: Int -> Int -> Int -> Par Integer
chunkfarm_sum_totient lower upper chunksize =
  sum <$> parMapChunkedNF chunksize clo_totient list
    where
      list = [upper, upper - 1 .. lower] :: [Int]


slicefarm_sum_totient :: Int -> Int -> Int -> Par Integer
slicefarm_sum_totient lower upper slices =
  sum <$> parMapSlicedNF slices clo_totient list
    where
      list = [upper, upper - 1 .. lower] :: [Int]


clo_totient :: Closure (Int -> Integer)
clo_totient = unsafeMkClosure0 val fun
  where val = totient
        fun = totient_Static

-}
-----------------------------------------------------------------------------
-- chunking up lists; inverse of 'chunk n' is 'concat'

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs where (ys,zs) = splitAt n xs

-----------------------------------------------------------------------------
-- slicing lists; inverse of 'slice n' is 'unslice'

slice :: Int -> [a] -> [[a]]
slice n = transpose . chunk n

unslice :: [[a]] -> [a]
unslice = concat . transpose


-----------------------------------------------------------------------------
-- initialisation, argument processing and 'main'

-- initialize random number generator
initrand :: Int -> IO ()
initrand seed = do
  when (seed /= 0) $ do
--    ranks <- MPI.allRanks
--    self <- MPI.myRank
--    let i = fromJust $ elemIndex self ranks
    setStdGen (mkStdGen (seed ))


-- parse runtime system config options (+ seed for random number generator)
-- parseOpts :: [String] -> (RTSConf, Int, [String])
-- parseOpts args = go (defaultRTSConf, 0, args) where
--   go :: (RTSConf, Int, [String]) -> (RTSConf, Int, [String])
--   go (conf, seed, [])   = (conf, seed, [])
--   go (conf, seed, s:ss) =
--    case stripPrefix "-rand=" s of
--    Just s  -> go (conf, read s, ss)
--    Nothing ->
--     case stripPrefix "-d" s of
--     Just s  -> go (conf { debugLvl = read s }, seed, ss)
--     Nothing ->
--      case stripPrefix "-scheds=" s of
--      Just s  -> go (conf { scheds = read s }, seed, ss)
--      Nothing ->
--       case stripPrefix "-wakeup=" s of
--       Just s  -> go (conf { wakeupDly = read s }, seed, ss)
--       Nothing ->
--        case stripPrefix "-hops=" s of
--        Just s  -> go (conf { maxHops = read s }, seed, ss)
--        Nothing ->
--         case stripPrefix "-maxFish=" s of
--         Just s  -> go (conf { maxFish = read s }, seed, ss)
--         Nothing ->
--          case stripPrefix "-minSched=" s of
--          Just s  -> go (conf { minSched = read s }, seed, ss)
--          Nothing ->
--           case stripPrefix "-minNoWork=" s of
--           Just s  -> go (conf { minFishDly = read s }, seed, ss)
--           Nothing ->
--            case stripPrefix "-maxNoWork=" s of
--            Just s  -> go (conf { maxFishDly = read s }, seed, ss)
--            Nothing ->
--             (conf, seed, s:ss)


-- parse (optional) arguments in this order: 
-- * version to run
-- * lower bound for Euler's totient function
-- * upper bound for Euler's totient function
-- * size of chunks (evaluated sequentially)
-- parseArgs :: [String] -> (Int, Int, Int, Int)
-- parseArgs []     = (defVers, defLower, defUpper, defChunk)
-- parseArgs (s:ss) =
--   let go :: Int -> [String] -> (Int, Int, Int, Int)
--       go v []           = (v, defLower, defUpper, defChunk)
--       go v [s1]         = (v, defLower, read s1,  defChunk)
--       go v [s1,s2]      = (v, read s1,  read s2,  defChunk)
--       go v (s1:s2:s3:_) = (v, read s1,  read s2,  read s3)
--   in case stripPrefix "v" s of
--        Just s' -> go (read s') ss
--        Nothing -> go defVers (s:ss)


defVers  =     7 :: Int
defLower =     1 :: Int
defUpper = 20000 :: Int
defChunk =   100 :: Int


timeIO action = do
   start <- getCurrentTime
   x     <- action
   end   <- getCurrentTime
   return (x, (fromRational$ toRational $ diffUTCTime end start) :: Double)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
--  registerStatic
--  MPI.defaultWithMPI $ do
  do 
    opts_args <- getArgs
--     let (conf, seed, args) = 
--           case opts_args of 
--              [c,s,a] -> (read c, read s, read a)

--    let (version, lower, upper, gran_arg) = parseArgs args

    -- From IFL paper:
    let (role,trans_, version, lower, upper, gran_arg) = 
          case opts_args of 
--	    [] -> ("master","tcp",2, 1, 10, 2)
	    [] -> ("master","tcp",2, 1, 65536, 128)
	    [r] -> (r,"tcp",2, 1, 65536, 128)
	    [r,t] -> (r,t,2, 1, 65536, 128)
	    [role,trans_,ver,lw,up,gran] -> (role,trans_, read ver, read lw, read up, read gran)
	    ls -> error$ "Unexpected number of arguments: "++ show ls
        seed = 3856
        trans = parse trans_
        parse "tcp"   = TCP
	parse "pipes" = Pipes

    initrand seed

    case role of
      "slave"  -> runParSlaveWithTransport [__remoteCallMetaData] trans
      "master" -> do
       case version of

{- 
      -- Serial version:
      "0" -> do (x, t) <- timeIO $ evaluate
                          (sum_totient [upper, upper - 1 .. lower])
                putStrLn $
		  "{v0} sum $ map totient [" ++ show lower ++ ".." ++
		  show upper ++ "] = " ++ show x ++
		  " {runtime=" ++ show t ++ "}"


      -- Plain SMP:
      1 -> do (output, t) <- timeIO $ evaluate =<< runParIO 
                               (par_sum_totient_chunked lower upper gran_arg)
              case output of
                Just x  -> putStrLn $
                             "{v1, chunksize=" ++ show gran_arg ++ "} " ++
                             "sum $ map totient [" ++ show lower ++ ".." ++
                             show upper ++ "] = " ++ show x ++
                             " {runtime=" ++ show t ++ "}"
                Nothing -> return ()

-}

          2 -> do (output, t) <- timeIO $ runParDistWithTransport [__remoteCallMetaData] trans $ 
                                      (dist_sum_totient_chunked lower upper gran_arg)
		  case output of
		   x  -> do putStrLn $
				"ANSWER: {v2, chunksize=" ++ show gran_arg ++ "} " ++
				"sum $ map totient [" ++ show lower ++ ".." ++
				show upper ++ "] = " ++ show x ++
				" {runtime=" ++ show t ++ "}"
                            putStrLn$ "SELFTIMED " ++ show t


{-
      3 -> do (output, t) <- timeIO $ evaluate =<< runParIO conf
                               (farm_sum_totient_chunked lower upper gran_arg)
              case output of
                Just x  -> putStrLn $
                             "{v3, chunksize=" ++ show gran_arg ++ "} " ++
                             "sum $ map totient [" ++ show lower ++ ".." ++
                             show upper ++ "] = " ++ show x ++
                             " {runtime=" ++ show t ++ "}"
                Nothing -> return ()
      4 -> do (output, t) <- timeIO $ evaluate =<< runParIO conf
                               (chunkfarm_sum_totient lower upper gran_arg)
              case output of
                Just x  -> putStrLn $
                             "{v4, chunksize=" ++ show gran_arg ++ "} " ++
                             "sum $ map totient [" ++ show lower ++ ".." ++
                             show upper ++ "] = " ++ show x ++
                             " {runtime=" ++ show t ++ "}"
                Nothing -> return ()
      5 -> do (output, t) <- timeIO $ evaluate =<< runParIO conf
                               (par_sum_totient_sliced lower upper gran_arg)
              case output of
                Just x  -> putStrLn $
                             "{v5, slices=" ++ show gran_arg ++ "} " ++
                             "sum $ map totient [" ++ show lower ++ ".." ++
                             show upper ++ "] = " ++ show x ++
                             " {runtime=" ++ show t ++ "}"
                Nothing -> return ()
      6 -> do (output, t) <- timeIO $ evaluate =<< runParIO conf
                               (dist_sum_totient_sliced lower upper gran_arg)
              case output of
                Just x  -> putStrLn $
                             "{v6, slices=" ++ show gran_arg ++ "} " ++
                             "sum $ map totient [" ++ show lower ++ ".." ++
                             show upper ++ "] = " ++ show x ++
                             " {runtime=" ++ show t ++ "}"
                Nothing -> return ()
      7 -> do (output, t) <- timeIO $ evaluate =<< runParIO conf
                               (farm_sum_totient_sliced lower upper gran_arg)
              case output of
                Just x  -> putStrLn $
                             "{v7, slices=" ++ show gran_arg ++ "} " ++
                             "sum $ map totient [" ++ show lower ++ ".." ++
                             show upper ++ "] = " ++ show x ++
                             " {runtime=" ++ show t ++ "}"
                Nothing -> return ()
      8 -> do (output, t) <- timeIO $ evaluate =<< runParIO conf
                               (slicefarm_sum_totient lower upper gran_arg)
              case output of
                Just x  -> putStrLn $
                             "{v8, slices=" ++ show gran_arg ++ "} " ++
                             "sum $ map totient [" ++ show lower ++ ".." ++
                             show upper ++ "] = " ++ show x ++
                             " {runtime=" ++ show t ++ "}"
                Nothing -> return ()
      _ -> return ()
-}
