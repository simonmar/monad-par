
module Main where

import qualified Data.Set as Set

import GHC.Conc (getNumProcessors)
import System.Environment (getEnvironment)
import System.IO.Unsafe (unsafePerformIO)


import HSBencher.Types(BenchSpace(..), Benchmark(..), ParamSetting(..), DefaultParamMeaning(..)
                       -- compileOptsOnly, enumerateBenchSpace, toCompileFlags,
                       -- makeBuildID, BuildID, 
                      )
import HSBencher.App (defaultMainWithBechmarks)

-- Temp:
-- import Text.PrettyPrint.GenericPretty (Out(doc,docPrec), Generic)

main :: IO ()
main = defaultMainWithBechmarks bls

--------------------------------------------------------------------------------
-- Here are the actual benchmarks:
--------------------------------------------------------------------------------

bls :: [Benchmark DefaultParamMeaning]
bls =
 ------------------------------------------------------------  
 -- Desktop configuration:
 ------------------------------------------------------------    
 [ Benchmark "src/blackscholes/blackscholes.hs" ["10000","15000000"]  futures
 , Benchmark "src/nbody/nbody.hs"               ["13000"]             ivars
 , Benchmark "src/mandel/mandel.hs"             ["1024","1024","256"] futures
 , Benchmark "src/coins/coins.hs"               ["8", "1250"]         futures

   -- These don't match the naming convention at the moment:
 , Benchmark "src/matmult/MatMult.hs"           ["768", "0", "64"]    futures   
 , Benchmark "src/sumeuler/sumeuler.hs"         ["38", "8000", "100"] futures
 , Benchmark "src/sorting/mergesort.hs"         ["cpu", "24", "8192"] futures
 ]

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

     -- # See README.md for format description.
     -- #
     -- # Version: server 1.5
     -- # 
     -- # I'm attempting to keep track of changes to this config with the above.
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

     -- src/blackscholes  futures 10000 30000000

     -- src/nbody         ivars  25000

     -- src/mandel futures 1024 1024 512

     -- src/coins         futures 8 1600

     -- src/matmult/MatMult   futures 1024 0 64

     -- src/sumeuler/sumeuler futures 38 16000 100

     -- src/sorting/mergesort futures cpu 24 8192



test_metapar :: Bool
test_metapar = False

--------------------------------------------------------------------------------
-- Set up some common benchmark config spaces:
--------------------------------------------------------------------------------

-- | Benchmarks that only require futures, not ivars.
futures :: BenchSpace DefaultParamMeaning
futures = defaultSettings$ varyThreads $
          Or$ map sched $ Set.toList defaultSchedSet

-- | Actually using ivars.  For now this just rules out the Sparks scheduler:
ivars :: BenchSpace DefaultParamMeaning
ivars   = defaultSettings$ varyThreads $
          Or$ map sched $ Set.toList $
          Set.delete Sparks defaultSchedSet

defaultSettings :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
defaultSettings spc =
  And [ Set NoMeaning (CompileParam "--disable-documentation")
      , Set NoMeaning (CompileParam "--disable-library-profiling")
      , Set NoMeaning (CompileParam "--disable-executable-profiling")
      , Set NoMeaning (RuntimeParam "+RTS -s -qa -RTS")
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
   = Trace | Direct | Sparks   -- Basic monad-par
   | SMP | NUMA                -- Meta-par
   | None
   -- | ContFree   -- Obsolete strawman.
 deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- | Realize a scheduler selection via a compile flag.
sched :: Sched -> BenchSpace DefaultParamMeaning
sched s = Set (Variant$ show s) $ CompileParam $ schedToCabalFlag s

-- | By default, we usually don't test meta-par.
defaultSchedSet :: Set.Set Sched
defaultSchedSet = Set.difference
                  (Set.fromList [minBound ..])
                  (if test_metapar
                   then Set.singleton None
                   else Set.fromList [NUMA, SMP, None])

-- TODO -- we really need to factor this out into a configuration file.
schedToModule :: Sched -> String
schedToModule s = 
  case s of 
--   Trace    -> "Control.Monad.Par"
   Trace    -> "Control.Monad.Par.Scheds.Trace"
   Direct   -> "Control.Monad.Par.Scheds.Direct"
   Sparks   -> "Control.Monad.Par.Scheds.Sparks"
   SMP      -> "Control.Monad.Par.Meta.SMP"
   NUMA     -> "Control.Monad.Par.Meta.NUMAOnly"
   None     -> "qualified Control.Monad.Par as NotUsed"

schedToCabalFlag :: Sched -> String
schedToCabalFlag s =
-- "--flags="  
  case s of
    Trace -> "-ftrace"
    Direct -> "-fdirect"
    Sparks -> "-fsparks"
    SMP -> "-fmeta-smp"
    NUMA -> "-fmeta-numa"
    None -> ""

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
  [ conf -- Unthreaded mode.
  , And [ Set NoMeaning (CompileParam "--ghc-options='-threaded'")
        , Or (map fn threadSelection)
        , conf ]
  ]
 where
   fn n = Set (Threads n) $ RuntimeParam  ("+RTS -N"++ show n++" -RTS")

