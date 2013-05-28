
module Main where

import qualified Data.Set as Set

import GHC.Conc (getNumProcessors)
import System.Environment (getEnvironment)
import System.IO.Unsafe (unsafePerformIO)


import HSBencher.Types(BenchSpace(..), Benchmark(..), ParamSetting(..),
                       compileOptsOnly, enumerateBenchSpace, toCompileFlags,
                       makeBuildID, BuildID, DefaultParamMeaning(..))
import HSBencher.App (defaultMainWithBechmarks)

-- Temp:
import Text.PrettyPrint.GenericPretty (Out(doc,docPrec), Generic)

main = defaultMainWithBechmarks bls

--------------------------------------------------------------------------------
-- Here are the actual benchmarks:
--------------------------------------------------------------------------------

bls = 
 [ Benchmark "src/blackscholes/blackscholes.hs" ["10000","15000000"]  futures
 -- , Benchmark "src/nbody/nbody.hs"               ["13000"]             ivars
 -- , Benchmark "src/mandel/mandel.hs"             ["1024","1024","256"] futures
 -- , Benchmark "src/coins/coins.hs"               ["8", "1250"]         futures

 --   -- These don't match the naming convention at the moment:
 -- , Benchmark "src/matmult/MatMult.hs"           ["768", "0", "64"]    futures   
 -- , Benchmark "src/sumeuler/sumeuler.hs"         ["38", "8000", "100"] futures
 -- , Benchmark "src/sorting/mergesort.hs"         ["cpu", "24", "8192"] futures
 ]

test_metapar :: Bool
test_metapar = False

--------------------------------------------------------------------------------
-- Set up some common benchmark config spaces:
--------------------------------------------------------------------------------

-- | Benchmarks that only require futures, not ivars.
futures = defaultSettings$ varyThreads $
          Or$ map sched $ Set.toList defaultSchedSet

-- | Actually using ivars.  For now this just rules out the Sparks scheduler:
ivars   = defaultSettings$ varyThreads $
          Or$ map sched $ Set.toList $
          Set.delete Sparks defaultSchedSet

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

