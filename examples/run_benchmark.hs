
{- | The benchmarking script:

USAGE:

   ./run_benchmark [mode] [hsbencher options]

Where mode is 'desktop', 'server', or 'quick'.

-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import qualified Data.Set as S

import GHC.Conc           (getNumProcessors)
import System.Environment (getEnvironment, getArgs, withArgs)
import System.IO.Unsafe   (unsafePerformIO)
import System.Console.GetOpt

import HSBencher.Types(BenchSpace(..), Benchmark(..), ParamSetting(..), DefaultParamMeaning(..),
                       -- compileOptsOnly, enumerateBenchSpace, toCompileFlags,
                       -- makeBuildID, BuildID,
                       mkBenchmark
                      )
import HSBencher.App (defaultMainWithBechmarks, all_cli_options)

--------------------------------------------------------------------------------
-- Main Script
--------------------------------------------------------------------------------

data Mode = Server | Desktop | Quick      deriving (Show,Eq)
data Flag = SetMode Mode
          | SetSched Sched
          | Help
          deriving (Show,Eq)
                
options :: [OptDescr Flag]
options =
     [ Option [] ["server"]  (NoArg (SetMode Server))  "server-sized benchmarks"
     , Option [] ["desktop"] (NoArg (SetMode Desktop)) "desktop-sized benchmarks"
     , Option [] ["quick"]   (NoArg (SetMode Quick))   "(default) quick testing"
     , Option ['h'] ["help"] (NoArg Help)              "report this help message"

     , Option [] ["sparks"]  (NoArg (SetSched Sparks)) "add this scheduler (default is all schedulers)"
     , Option [] ["direct"]  (NoArg (SetSched Direct)) "add this scheduler "       
     , Option [] ["trace"]   (NoArg (SetSched Trace))  "add this scheduler "
     , Option [] ["trace-st"]   (NoArg (SetSched TraceST)) "scheduler with one transformer"
       
     , Option [] ["lvish"]   (NoArg (SetSched LVish))  "add this scheduler "
     , Option [] ["lvish-state"] (NoArg (SetSched LVishState)) "scheduler with one transformer"
     , Option [] ["lvish-rng"] (NoArg (SetSched LVishRNG)) "scheduler with one transformer"
     , Option [] ["lvish-cancel"] (NoArg (SetSched LVishCancel)) "scheduler with one transformer"        
     ]

isSetSched (SetSched _) = True
isSetSched _ = False

main :: IO ()
main = do
  args <- getArgs
  let (opts,nonopts,unrecog,errs) = getOpt' Permute options args
  -- The first arg is a kind of mode:

  let help1 = usageInfo ("USAGE: run_benchmark [options]\n"++
                        "\nFirst, specific options for this script are:\n")
                options
      help2 = usageInfo (help1++"\nAlso use the generic HSBencher options below:\n")
                        (concat $ map snd all_cli_options)

      activeScheds0 = [ sched | SetSched sched <- opts ]
      activeScheds = if null activeScheds0
                     then defaultSchedSet
                     else S.fromList activeScheds0
  if Help `elem` opts || errs /= [] then
    error help2
   else do
    let passthru = nonopts ++ unrecog
        modes    = [ s | SetMode s <- opts ]
    putStrLn$ "  [Bench script mode selection]: "++ show modes
    putStrLn$ "  [Bench script Sched selection]: "++ show activeScheds
    putStrLn$ "  [Note: passing through options to HSBencher]: "++unwords passthru
    withArgs passthru $ 
     case modes of
       [Desktop] -> defaultMainWithBechmarks (bls_desktop activeScheds)
       [Server]  -> defaultMainWithBechmarks (bls_server activeScheds)
       [Quick]   -> defaultMainWithBechmarks (bls_quick activeScheds)
       []        -> defaultMainWithBechmarks (bls_quick activeScheds)
       ls        -> error$ "Conflicting mode options: "++show ls
    
--------------------------------------------------------------------------------
-- Here are the actual benchmarks:
--------------------------------------------------------------------------------

bls_quick :: S.Set Sched -> [Benchmark DefaultParamMeaning]
bls_quick ss =
 ------------------------------------------------------------  
 -- Quick-test configuration:
 ------------------------------------------------------------    
 [ futbench "blackscholes" [] ss 
 , ivbench  "nbody"        [] ss 
 , futbench "mandel"       [] ss 
 , futbench "coins"        [] ss 

   -- These don't match the naming convention at the moment:
 , futbench "matmult"      [] ss 
 , futbench "sumeuler"     [] ss 
 , futbench "sorting"      [] ss 
 ]

bls_desktop :: S.Set Sched -> [Benchmark DefaultParamMeaning]
bls_desktop ss = 
 ------------------------------------------------------------  
 -- Desktop configuration:
 ------------------------------------------------------------
 [ futbench "blackscholes" ["10000","15000000"] ss 
 , ivbench  "nbody"        ["13000"] ss 
 , futbench "mandel"       ["1024","1024","256"] ss 
 , futbench "coins"        ["8", "1250"] ss 

   -- These don't match the naming convention at the moment:
 , futbench "matmult"      ["768", "0", "64"] ss 
 , futbench "sumeuler"     ["38", "8000", "100"] ss 
 , futbench "sorting"      ["cpu", "24", "8192"] ss 
 ] ++ parfibRange ss

-- | Parfib can vary wildly between schedulers. Thus we run it with a short timeout,
-- and then we can simply look at what the maximum size each scheduler could handle
-- under that threshold.
parfibRange :: S.Set Sched -> [Benchmark DefaultParamMeaning]
parfibRange ss =
  [ (mkBenchmark ("src/parfib/generated.cabal") ["monad",arg] (futures ss)) 
      { progname= Just "microbench_parfib"
      , benchTimeOut= Just 6.0 }
  | arg <- map show [30,31,32,33,34,35,36,37,38,39,40]
  ]

-- # Version: server 1.6
-- # I'm attempting to keep track of changes to this config with the above number.
-- # Note that changes to the benchmarks themselves also require changing
-- # this version number.  However, ADDING new benchmarks does not require 
-- # a version bump.
-- # 
-- # CHANGELOG:
-- # 
-- # 1.2 - bringing up to date with benchlist.txt for paper.
-- # 1.3 - changed mandel implementation
-- # 1.4 - removed path prefixes for new cabal build system -ACF
-- # 1.5 - prefixes back
-- # 1.6 - up mergesort 24->25
bls_server :: S.Set Sched -> [Benchmark DefaultParamMeaning]
bls_server ss =
 [ futbench "blackscholes" ["10000","30000000"] ss 
 , ivbench  "nbody"        ["25000"] ss 
 , futbench "mandel"       (words "1024 1024 512") ss 
 , futbench "coins"        ["8", "1600"] ss 

   -- These don't match the naming convention at the moment:
 , futbench "matmult"      (words "1024 0 64") ss 
 , futbench "sumeuler"     (words "38 16000 100") ss 
 , futbench "sorting"      ["cpu", "25", "8192"] ss 
 ] ++ parfibRange ss

-- Factor out boilerplate:

futbench :: String -> [String] -> S.Set Sched -> Benchmark DefaultParamMeaning
futbench dir args ss =
   (mkBenchmark ("src/"++dir++"/generated.cabal")  args  (futures ss)) { progname=Just dir }   

ivbench :: String -> [String] -> S.Set Sched -> Benchmark DefaultParamMeaning
ivbench dir args ss =
   (mkBenchmark ("src/"++dir++"/generated.cabal")  args  (ivars ss)) { progname=Just dir }   


----------------------------------------
-- Old, disabled benchmarks:
----------------------------------------

     -- binomial lattice?

     -- ------------------------------------------------------------
     -- TODO: Get distributed benchmarks integrated in here:
     --       Although these should probably go in a separate file.
     -- ------------------------------------------------------------
     -- distributed/parfib_dist
     -- distributed/mandel_dist dist pipes 1024a

     -- ------------------------------------------------------------
     -- Benchmarks that are have problems or have become neglected:
     -- ------------------------------------------------------------

     -- partree/partree futures 600 20

     -- What should the arguments be here:
     --  The compute times are highly unpredictable.... it's hard to find good inputs.
     -- minimax/minimax futures nested ? ? 
     -- minimax/minimax futures monad  ? ? 


     -- We can measure parfib separately... it shouldn't be part of our
     -- benchmark suite.
     -- -----------------
     -- Problems with this fib(38):
     -- parfib_monad  futures monad  38
     -- parfib_pseq   none           38
     -- parfib_monad  futures nested 30


     -- I don't think quicksort was every really fixed/tuned:
     -- It looks like it is still generating random data as an AList and
     -- getting stack overflows [2012.03.04]
     -- -----------------
     -- quicksort/parquicksort_monad futures 1500000
     -- quicksort/parquicksort_pseq  none    1500000


     -- We could include cholesky, but it is a bit annoying in that it
     -- requires generated input files.  Also we'd like to de-unsafePerformIO it:
     -- -----------------
     -- cholesky      default 1000 50 cholesky_matrix1000.dat


     -- The whole asynchronous/streaming thing needs to be revisited at a later date.
     -- -----------------
     -- stream/disjoint_working_sets_pipeline Trace  monad  4 256 10 10000sx
     -- stream/disjoint_working_sets_pipeline none   sparks 4 256 10 10000

------------------------------------------------------------  
-- Server configuration:
------------------------------------------------------------    



test_metapar :: Bool
test_metapar = False

--------------------------------------------------------------------------------
-- Set up some common benchmark config spaces:
--------------------------------------------------------------------------------

-- | Benchmarks that only require futures, not ivars.
futures :: S.Set Sched -> BenchSpace DefaultParamMeaning
futures ss = defaultSettings$ varyThreads $
          Or$ map dosched $ S.toList ss

-- | Actually using ivars.  For now this just rules out the Sparks scheduler:
ivars :: S.Set Sched -> BenchSpace DefaultParamMeaning
ivars ss = defaultSettings$ varyThreads $
          Or$ map dosched $ S.toList $
          S.delete Sparks ss  -- This is the only one that can't support IVars.

defaultSettings :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
defaultSettings spc =
  And [
--        Set NoMeaning (CompileParam "--disable-documentation")
--      , Set NoMeaning (CompileParam "--disable-library-profiling")
--      , Set NoMeaning (CompileParam "--disable-executable-profiling")
        Or [
             -- 512K is the default:
             Set NoMeaning (RuntimeParam "+RTS -s -qa -RTS") -- -A512K
             -- Set NoMeaning (RuntimeParam "+RTS -s -qa -A20M -RTS")
           ]
      , spc]

      -- rts = gc_stats_flag ++" "++
      --       case numthreads of
      --        0 -> unwords (pruneThreadedOpts (words ghc_RTS))
      --        _ -> ghc_RTS  ++" -N"++show numthreads

     --, ghc_RTS    =       get "GHC_RTS"   ("-qa " ++ gc_stats_flag) -- Default RTS flags.


--------------------------------------------------------------------------------
-- Supporting definitions:
--------------------------------------------------------------------------------

-- | Monad par schedulers:
data Sched 
   = Trace | TraceST 
   | Direct | Sparks   -- Basic monad-par
   | SMP | NUMA                -- Meta-par
   | LVish
   | LVishRNG
   | LVishState -- Add transformers...
   | LVishCancel  
   | None
   -- | ContFree   -- Obsolete strawman.
 deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- | Realize a scheduler selection via a compile flag.
dosched :: Sched -> BenchSpace DefaultParamMeaning
dosched s = Set (Variant$ show s) $ CompileParam $ schedToCabalFlag s

-- | By default, we usually don't test meta-par 
defaultSchedSet :: S.Set Sched
defaultSchedSet = S.difference
                  (S.fromList [minBound ..])
                  (if test_metapar
                   then S.fromList [None]
                   else S.fromList [NUMA, SMP, None])

schedToCabalFlag :: Sched -> String
schedToCabalFlag s =
  case s of
    Trace  -> "-ftrace"
    TraceST -> "-ftrace-st"
    Direct -> "-fdirect"
    Sparks -> "-fsparks"
    SMP    -> "-fmeta-smp"
    NUMA   -> "-fmeta-numa"
    LVish  -> "-flvish"
    LVishRNG   -> "-flvish-rng"
    LVishCancel   -> "-flvish-cancel" 
    LVishState -> "-flvish-state"
    None   -> ""

-- TODO: make this an option:
threadSelection :: [Int]
threadSelection = unsafePerformIO $ do
  env <- getEnvironment
  p   <- getNumProcessors
  case lookup "THREADS" env of
    Just ls -> return$ map read $ words ls
    -- Arbitrary default policy 
    Nothing
      | p <= 16   -> return  [1 .. p]
      | otherwise -> return$ 1 : [2,4 .. p]

-- unsafeEnv = unsafePerformIO getEnvironment

-- | Add variation from thread count.    
varyThreads :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
varyThreads conf = Or
  [
    -- Disabling unthreaded mode:
    -- conf -- Unthreaded mode.
    And [ Set NoMeaning (CompileParam "--ghc-options='-threaded'")
        , Or (map fn threadSelection)
        , conf ]
  ]
 where
   fn n = Set (Threads n) $ RuntimeParam  ("+RTS -N"++ show n++" -RTS")

