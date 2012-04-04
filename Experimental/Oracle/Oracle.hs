;import Control.Monad.Par
import System.Mem.StableName

data AbstCost = AC Float
data TimeCost = TC Float
data Resource = Seq | CPU | GPU | Dist
data KappaRange = KLo TimeCost | KUp TimeCost | KRange TimeCost TimeCost


data Name a b = IO (StableName (a -> Par b))

data Estimator = Est {
    init :: IO (),
    reportTime  :: AbstCost -> TimeCost -> IO (),
    predictTime :: AbstCost -> IO TimeCost
  -- There is implicitly state for this "object" that tracks the
  -- current prodediction in terms of scheduler overhead.  

  -- In the paper kappa is set as a constant.  The oracleFork will
  -- compare the predicted cost against kappa to determine if it
  -- should go ahead.
}

data ResourceImpl a b = RI {
    name :: Name a b,
    resourceType :: Resource,
    run :: a -> Par b,
    cost :: a -> AbstCost,
    kappa :: KappaRange
}

data ResourceImplSet a b = RISet {
    impls :: [ResourceImpl a b],
    estimator :: Estimator
}

-- I don't know what type this should actually be (probably not String...). 
--   (RRN:  See stringtable-atom...)
-- I want a table that has each stable name associated with a ResourceImplSet
data LookupTable = Map Name RISet
data StateInfo = SI {
    funTable :: LookupTable
     -- do we need a KappaRange here?
}

-- Should just take a name and arguments, ideally
-- For now, I guess I'll assume there's a ResourceImplSet passed in until I
-- understand where to put and how to get this from a stored state
oracleFork :: ResourceImplSet a b -> Name a b -> a -> Par b
oracleFork = undefined 

--------------------------------------------------------------------------------

cpuVer = RI {
    name = "foo_cpuVer" :: Name a b,
    resourceType = CPU :: Resource,
    run = \a -> mycode a 
    cost = \n -> (length n)^2
}

gpuVer = RI {
    name = "foo_gpuVer" :: Name a b,
    resourceType = GPU :: Resource,
--    run = mycode ... :: a -> Par (Acc b),
    run = (\a -> gpuSpawn (mycode a)) :: a -> Par b,   
    cost = (\n -> length n) :: a -> AbstCost,
--    kappa = ???? :: KappaRange
}





--------------------------------------------------------------------------------
-- How would we actually write the final function?
--------------------------------------------------------------------------------

-- This is the global registry (*explicit* naming) approach:

  do ... 
     result <- oracleFork (Name "foo") 39
     ... 

-- Or oracleFork could use the stableName itself to look up the entry
-- for that function:
     result <- oracleFork foo 39

-- But if you did that then somewhere else you would have to do something like this:
    register foo GPU  foo_gpu
    register foo Dist foo_dist


-- The abov do NOT include the ResourceImplSet.  It could be threaded
-- through with a StateT, perhaps.

  myRIs = (RISet [cpuVer,gpuVer])

-- If we explicitly initialized the state and associated it with a
-- function maybe that would look something like this:
  do myfoo <- makeEstimatorMagic (RISet [cpuVer,gpuVer,distVer])
     ...
     oracleFork myfoo 39  
     oracleFork myfoo 40 

     -- Remove distributed:
     myfoo2 <- makeEstimatorMagic (RISet [cpuVer,gpuVer])
     oracleFork myfoo2 41

--------------------------------------------------------------------------------
-- More REAL examples: write out a mergesort:
--------------------------------------------------------------------------------

-- Here's ONE way it could work, with OracleFun's passed by value, including
-- subfunction tables (i.e. not represented at the type level).

cost = O_NlogN

-- OracleFun would have IMPLICIT state, which is mutated, perhaps with
-- unsafePerformIO, to keep track of estimators.
superMergeSort :: OracleFun (V.Vector ElmT) (V.Vector ElmT)
superMergeSort = mkOracleFun [(GPU,  gpuMergeSort,  cost),
			      (Dist, distMergeSort, cost)
                              (Par,  parMergeSort,  cost)
                              (Ser,  serQuicksort,  cost)
			     ]

-- Merge sort for a Vector using the Par monad
-- t is the threshold for using sequential merge (see merge)
oracleMergeSort :: V.Vector ElmT -> Par (V.Vector ElmT)
oracleMergeSort vec | singleton? vec = ...
oracleMergeSort vec = 
   do let n = (V.length vec) `div` 2
      let (lhalf, rhalf) = V.splitAt n vec
      ileft <- oracleSpawn$ superMergeSort lhalf
      right <-              oracleMergeSort rhalf
      left  <- get ileft
      merge left right

parMergeSort :: V.Vector ElmT -> Par (V.Vector ElmT)
parMergeSort vec | singleton? vec = ...
parMergeSort vec = 
   do let n = (V.length vec) `div` 2
      let (lhalf, rhalf) = V.splitAt n vec
      ileft <- spawn$ parMergeSort    lhalf
      right <-        oracleMergeSort rhalf
--      right <-        parMergeSort rhalf
      left  <- get ileft
      merge left right

distMergeSort :: V.Vector ElmT -> Par (V.Vector ElmT)
distMergeSort vec | singleton? vec = ...
distMergeSort vec = 
   do let n = (V.length vec) `div` 2
      let (lhalf, rhalf) = V.splitAt n vec
      ileft <- longSpawn$ $(mkClosure distMergeSort) lhalf
      right <-            oracleMergeSort rhalf
      left  <- get ileft
      merge left right

$[remotable 'distMergeSort]

-- QUESTION -- can the above three versions be FACTORED into one
-- recursive function that is parameterized by the left-spawn.

-- No recursion through the oracle
gpuMergeSort vec = runAcc (...)

-- These are BOTH effectively LEAVES:
serMergeSort = ...

------------------------------------------------------------
-- Then.... in the real program:

main = ....
       oracleMergeSort myVec
       -- OR
       oracleSpawn superMergeSort myVec
       ....


--------------------------------------------------------------------------------

-- recursiveMergSort mySpawn

recursiveMergeSort :: ??? -> V.Vector ElmT -> Par (V.Vector ElmT)
recursiveMergeSort mySpawn vec | singleton? vec = ...
recursiveMergeSort mySpawn vec = 
   do let n = (V.length vec) `div` 2
      let (lhalf, rhalf) = V.splitAt n vec
      ileft <- mySpawn            lhalf
      right <- recrusiveMergeSort rhalf
      left  <- get ileft
      oracleSpawn superMerge (left,right) 

superMergeSort = mkOracleFun [
			      (Dist, recursiveMergeSort (longSpawn$ $(mkClosure ...)) , cost),
                              (Par,  recursiveMergeSort (spawn . recursiveMergeSort) ,  cost),
                              -- Will this MULTIWAY mergesort look ANY DIFFERENT to the oracle?
                              (Par,  recursiveMULTIWAYAMergeSort (spawn . recursiveMULTIWAYAMergeSort) ,  cost),
                              -- (Ser,  recursiveMergeSort recursiveMergeSort,  cost),
			      -- Leaves:
			      (GPU,  gpuMergeSort,  cost),
                              (Ser,  serQuicksort,  cost)
			      -- Do I trust the system enough to give it both Ser's 
			      -- and let the oracle sort it out.
			     ]

superMerge = mkOracleFun [(Par, ...),
			  (Ser, ...)]

----------------------------------------------------------------------------------------------------
-- Alternative Vision - introduce a unique function for each type:

data MyMergeSort = MyMergeSort

instance OracleFun MyMergeSort (V.Vector ElmT) (V.Vector ElmT) where 
  implList MyMergeSort = [(Dist,...), (Par,...)]

main = ...
       oracleSpawn MyMergeSort myVec
       ...

