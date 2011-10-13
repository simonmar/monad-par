#!/usr/bin/env runhaskell
{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, RecordWildCards #-}

-- NOTE: Under 7.2 I'm running into this HSH problem:

-- benchmark.hs: HSH/Command.hs:(289,14)-(295,45): Missing field in record construction System.Process.Internals.create_group

{- 
   Runs benchmarks.  (Note Simon Marlow has his another setup, but this
   one is self contained.)

   ---------------------------------------------------------------------------
   Usage: [set env vars] ./benchmark.hs

   Call it with the following environment variables...

     SHORTRUN=1 to get a shorter run for testing rather than benchmarking.

     THREADS="1 2 4" to run with # threads = 1, 2, or 4.

     KEEPGOING=1 to keep going after the first error.

     TRIALS=N to control the number of times each benchmark is run.

     BENCHLIST=foo.txt to select the benchmarks and their arguments
		       (uses benchlist.txt by default)

     SCHEDS="Trace Direct Sparks"  (or a subset thereof)

   Additionally, this script will propagate any flags placed in the
   environment variables $GHC_FLAGS and $GHC_RTS.  It will also use
   $GHC, if available, to select the $GHC executable.

   ---------------------------------------------------------------------------
-}

import System.Environment
import System.Directory
import System.Exit
import System.FilePath (splitFileName)
import System.Process (system)
--import GHC.Conc (numCapabilities)
import HSH
import Prelude hiding (log)
import Control.Monad.Reader
import Text.Printf
import Debug.Trace
import Data.Char (isSpace)
import Data.List (isPrefixOf, tails)

-- The global configuration for benchmarking:
data Config = Config 
 { benchlist      :: [(String,[String])]
 , threadsettings :: [Int]  -- A list of #threads to test.  0 signifies non-threaded mode.
 , maxthreads     :: Int
 , trials         :: Int    -- number of runs of each configuration
 , shortrun       :: Bool
 , keepgoing      :: Bool   -- keep going after error
 , ghc            :: String -- ghc compiler path
 , ghc_flags      :: String
 , ghc_RTS        :: String -- +RTS flags

 , hostname       :: String 
 , resultsFile    :: String -- Where to put timing results.
 , logFile        :: String -- Where to put more verbose testing output.
 }

-- TODO: Add more systematic management of the configuration of
-- individual runs (number of threads, other flags, etc):
data BenchSettings = BenchSettings

-- Name of a script to time N runs of a program:
-- (I used a haskell script for this but ran into problems at one point):
-- ntimes = "./ntimes_binsearch.sh"
ntimes = "./ntimes_minmedmax"

--------------------------------------------------------------------------------
-- Configuration

-- Retrieve the configuration from the environment.
getConfig = do
  hostname <- runSL$ "hostname -s"
  env      <- getEnvironment

  let get v x = case lookup v env of 
		  Nothing -> x
		  Just  s -> s

      bench = get "BENCHLIST" "benchlist.txt"
      logFile = "bench_" ++ hostname ++ ".log"
      shortrun = strBool (get "SHORTRUN"  "0")
  -- We can't use numCapabilities as the number of hardware threads
  -- because this script may not be running in threaded mode.

  ----------------------------------------
  -- Determine the number of cores.
  d <- doesDirectoryExist "/sys/devices/system/cpu/"
  uname <- runSL "uname"
  maxthreads :: String 
       <- if d 
	  then runSL$ "ls  /sys/devices/system/cpu/" -|- egrep "cpu[0123456789]*$" -|- wcL
	  else if uname == "Darwin"
	  then runSL$ "sysctl -n hw.ncpu"
	  else error$ "Don't know how to determine the number of threads on platform: "++ show uname
                -- TODO: Windows!
  -- Note -- how do we find the # of threads ignoring hyperthreading?
  ----------------------------------------

  logOn logFile$ "Reading list of benchmarks/parameters from: "++bench
  benchstr <- readFile bench

  -- Here are the DEFAULT VALUES:
  return$ 
    Config { hostname
	   , logFile  
	   , ghc        =       get "GHC"       "ghc"
	   , ghc_RTS    =       get "GHC_RTS"   "-qa -s"
  	   , ghc_flags  = (get "GHC_FLAGS" (if shortrun then "" else "-O2")) 
	                  ++ " -rtsopts" -- Always turn on rts opts.
	   , trials     = read$ get "TRIALS"    "1"
	   , benchlist  = parseBenchList benchstr
	   , maxthreads = read maxthreads
	   , threadsettings = parseList$ get "THREADS" maxthreads
	   , shortrun    
	   , keepgoing   = strBool (get "KEEPGOING" "0")
	   , resultsFile = "results_" ++ hostname ++ ".dat"
	   }

pruneThreadedOpts :: [String] -> [String]
--pruneThreadedOpts = filter (`notElem` ["-qa", "-qb"])
pruneThreadedOpts ls = trace ("PRUNED of "++show ls++" was "++ show x)x 
 where 
  x = filter (`notElem` ["-qa", "-qb"]) ls

--------------------------------------------------------------------------------
-- Misc Small Helpers

parseList :: String -> [Int]
parseList = map read . words 

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

slidingWin w ls = 
  reverse $ map reverse $ 
  filter (not . null) $ 
--  filter ((== w) . length) $ 
--  map (take w) (tails ls)
  map (take w) (tails$ reverse ls)


parseBenchList str = 
--   trace (" GOT LIST "++ show ls) $ 
   map (\ (h:t) -> (h,t)) ls
 where 
 ls = 
  filter (not . null) $
  map words $ 
  filter (not . isPrefixOf "#") $  -- filter comments
  map trim $
  lines str

strBool ""  = False
strBool "0" = False
strBool "1" = True
strBool  x  = error$ "Invalid boolean setting for environment variable: "++x

inDirectory dir action = do
  d1 <- liftIO$ getCurrentDirectory
  liftIO$ setCurrentDirectory dir
  action
  liftIO$ setCurrentDirectory d1
  
--------------------------------------------------------------------------------

-- Check the return code from a call to a test executable:
check ExitSuccess _           = return ()
check (ExitFailure code) msg  = do
  Config{..} <- ask
  let report = log$ printf " #      Return code %d Params: %s, RTS %s " (143::Int) ghc_flags ghc_RTS
  case code of 
   143 -> 
     do report
        log         " #      Process TIMED OUT!!" 
   _ -> 
     do log$ " # "++msg 
	report 
        log "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        unless keepgoing $ 
          lift$ exit code


--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

log :: String -> ReaderT Config IO ()
log str = do
  Config{logFile} <- ask
  lift$ logOn logFile str

logOn :: String -> String -> IO ()
logOn file s = 
  runIO$ echo (s++"\n") -|- tee ["/dev/stdout"] -|- appendTo file

backupResults Config{resultsFile, logFile} = do 
  e    <- doesFileExist resultsFile
  --  date <- runSL "date +%s"
  date <- runSL "date +%Y%m%d_%R"
  when e $ do
    renameFile resultsFile (resultsFile ++"."++date++".bak")
  e2   <- doesFileExist logFile
  when e2 $ do
    renameFile logFile     (logFile     ++"."++date++".bak")

--------------------------------------------------------------------------------
-- Running Benchmarks
--------------------------------------------------------------------------------

-- If the benchmark has already been compiled doCompile=False can be
-- used to skip straight to the execution.
runOne :: Bool -> String -> [String] -> Int -> (Int,Int) -> ReaderT Config IO ()
runOne doCompile test args numthreads (iterNum,totalIters) = do
  
  log$ "\n--------------------------------------------------------------------------------"
  log$ "  Running Config "++show iterNum++" of "++show totalIters++
       ": "++test++" (args \""++unwords args++"\") scheduler ?  threads "++show numthreads
  log$ "--------------------------------------------------------------------------------\n"

  -- pwd <- runSL "pwd"
  pwd <- lift$ getCurrentDirectory
  log$ "(In directory "++ pwd ++")"

  Config{..} <- ask

  -- numthreads == 0 indicates a serial run:
  let (rts,flags) = case numthreads of
		     0 -> (ghc_RTS, ghc_flags)
		     _ -> (ghc_RTS  ++" -N"++show numthreads, 
			   ghc_flags++" -threaded")

      (containingdir,_) = splitFileName test
      hsfile = test++".hs"
      tmpfile = "._Temp_output_buffer.txt"
      flushtmp = do lift$ runIO$ catFrom [tmpfile] -|- indent -|- appendTo logFile
		    lift$ runIO$ catFrom [tmpfile] -|- indent -- To stdout
		    lift$ removeFile tmpfile
      -- Indent for prettier output
      indent = map ("    "++)

  ----------------------------------------
  -- Do the Compile
  ----------------------------------------
  when doCompile $ do 

     e  <- lift$ doesFileExist hsfile
     d  <- lift$ doesDirectoryExist containingdir
     mf <- lift$ doesFileExist$     containingdir ++ "/Makefile"
     if e then do 
	 log "Compiling with a single GHC command: "
	 let cmd = unwords [ghc, "-i../", "-i"++containingdir, flags ++ " -fforce-recomp", 
			    hsfile, "-o "++test++".exe"]		
	 log$ "  "++cmd ++"\n"
	 -- Having trouble getting the &> redirection working.  Need to specify bash specifically:
	 code <- lift$ system$ "bash -c "++show (cmd++" &> "++tmpfile)
	 flushtmp 
	 check code "ERROR, benchmark.hs: compilation failed."


     else if (d && mf && containingdir /= ".") then do 
	log " ** Benchmark appears in a subdirectory with Makefile.  Using it."
	log " ** WARNING: Can't currently control compiler options for this benchmark!"
	inDirectory containingdir $ do 
	   code <- lift$ system "make"
	   check code "ERROR, benchmark.hs: Compilation via benchmark Makefile failed:"

     else do 
	log$ "ERROR, benchmark.hs: File does not exist: "++hsfile
	lift$ exit 1

  ----------------------------------------
  -- Done Compiling, now Execute:
  ----------------------------------------

  let prunedRTS = unwords (pruneThreadedOpts (words rts)) -- ++ "-N" ++ show numthreads
      ntimescmd = printf "%s %d ./%s.exe %s +RTS %s -RTS" ntimes trials test (unwords args) prunedRTS
  log$ "Executing " ++ ntimescmd

  -- One option woud be dynamic feedback where if the first one
  -- takes a long time we don't bother doing more trials.  

--  if [ "$SHORTRUN" != "" ]; then export HIDEOUTPUT=1; fi

  -- NOTE: With this form we don't get the error code.  Rather there will be an exception on error:
  (str::String,finish) <- lift$ run (ntimescmd ++" 2> "++ tmpfile) -- HSH can't capture stderr presently
  (_  ::String,code)   <- lift$ finish                       -- Wait for child command to complete
  flushtmp 
  check code ("ERROR, benchmark.hs: test command \""++ntimescmd++"\" failed with code "++ show code)

  log$ " >>> MIN/MEDIAN/MAX TIMES " ++ 
     case code of
      ExitSuccess     -> str
      ExitFailure 143 -> "TIMEOUT TIMEOUT TIMEOUT"
      ExitFailure _   -> "ERR ERR ERR"
		     

  return ()
  

--------------------------------------------------------------------------------

resultsHeader :: Config -> IO ()
resultsHeader Config{ghc, trials, ghc_flags, ghc_RTS, maxthreads, resultsFile, logFile} = 
  mapM_ runIO $ 
--  map (-|- appendTo results) $
  [
    e$ "# TestName Variant NumThreads   MinTime MedianTime MaxTime"        
  , e$ "#    "        
  , e$ "# `date`"
  , e$ "# `uname -a`" 
  , e$ "# Determined machine to have "++show maxthreads++" hardware threads."
  , e$ "# `ghc -V`" 
  , e$ "# "                                                                
  , e$ "# Running each test for "++show trials++" trial(s)."
  , e$ "#  ... with compiler options: " ++ ghc_flags
  , e$ "#  ... with runtime options: " ++ ghc_RTS
  , e$ "# Using the following settings from the benchmarking environment:" 
  , e$ "# BENCHLIST=$BENCHLIST  THREADSETTINGS=$THREADSETTINGS  TRIALS=$TRIALS  SHORTRUN=$SHORTRUN  KEEPGOING=$KEEPGOING  GHC=$GHC  GHC_FLAGS=$GHC_FLAGS  GHC_RTS=$GHC_RTS"
  ]
 where 
    e s = ("echo \""++s++"\"") -|- tee ["/dev/stdout", logFile] -|- appendTo resultsFile


----------------------------------------------------------------------------------------------------
-- Main Script
----------------------------------------------------------------------------------------------------

main = do

  -- HACK: with all the inter-machine syncing and different version
  -- control systems I run into permissions problems sometimes:
  system "chmod +x ./ntime* ./*.sh"

  conf@Config{..} <- getConfig    

  runReaderT 
    (do         

        lift$ backupResults conf
	log "Writing header for result data file:"
	lift$ resultsHeader conf 

	log "Before testing, first 'make clean' for hygiene."
	-- Hah, make.out seems to be a special name of some sort, this actually fails:
	--	code <- lift$ system$ "make clean &> make.out"
	code <- lift$ system$ "make clean &> make_output.tmp"
	check code "ERROR: 'make clean' failed."
	log " -> Succeeded."
	liftIO$ removeFile "make_output.tmp"

        let total = length threadsettings * length benchlist
        log$ "\n--------------------------------------------------------------------------------"
        log$ "Running all benchmarks for all thread settings in "++show threadsettings
        log$ "Testing "++show total++" total configurations of "++ show (length benchlist) ++" benchmarks"
        log$ "--------------------------------------------------------------------------------"
        
        forM_ (zip [1..] benchlist) $ \ (n, (test,args)) -> 
          forM_ (slidingWin 2 threadsettings) $ \ win -> do
              let recomp = case win of 
			     [x]   -> True -- First run, must compile.
			     -- Switching from serial to parallel or back requires recompile:
			     [0,t] | t > 0 -> True 
			     [t,0] | t > 0 -> True 
			     -- Otherwise, no recompile:
			     [last,current] -> False
		  numthreads = head$ reverse win
              when recomp $ log "First time or changing threading mode, recompile required:"
	      runOne recomp test (if shortrun then [] else args) numthreads (n,total)

        log$ "\n--------------------------------------------------------------------------------"
        log "  Finished with all test configurations."
        log$ "--------------------------------------------------------------------------------"
    )
    conf


