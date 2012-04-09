{-# LANGUAGE MultiParamTypeClasses, CPP #-}

module Oracle where

import Control.Monad.Par
import System.Mem.StableName

import qualified Data.Vector.Storable as V
import Data.Vector.Algorithms.Intro (sort)
import Data.Word (Word32)

data Resource = Seq | CPU | GPU | Dist
type AbstCost = Float
type FunImpl a b = (Resource, a -> Par b, a -> AbstCost)
type Name a = IO (StableName a)

{-
 - 1. Create Estimator for a
 - 2. Insert function implementations and estimator into Map at index generated 
 -      from Name a (hashStableName a)
 -
 - TODO: What if Name a is already in the table?
 -}
mkOracleFun :: Name a -> [FunImpl a b] -> Par ()
mkOracleFun = undefined
    {-
     - do
     -   est <- mkEstimator name funs
     -   table <- getTable
     -   insert table name (est, funs)
     -   ...
     -}


-- Really want a dependent type here, such that when Name is looked up in the 
-- Map we get functions of type b -> Par c
{-
 - 1. Look up name in map: (estimator, funImpls)
 - 2. Apply cost functions
 - 3. Use estimator to predict costs (in funImpls)
 - 4. Compare to kappas, choose funImpl
 -
 - TODO: Does the oracle just apply function provided by user, or does it 
 -  make a call based on the resource selected? E.g., adding the mkClosure 
 -  for distributed (is that even possible for the oracle to do?).
 -}
oracleSpawn :: Name a -> b -> Par (IVar c)
oracleSpawn = undefined


------------------------------------------------------------------------------
-- User input
------------------------------------------------------------------------------

type ElmT = Word32

#ifndef SAFE
thawit x = V.unsafeThaw x
#else
thawit x = V.thaw x
#endif

-- Sequential mergesort
seqMergesort :: V.Vector ElmT -> Par (V.Vector ElmT)
seqMergesort vec = return $ V.create $ do
                    mut <- thawit vec
                    sort mut
                    return mut

-- Mergesort cost: n lg n (where n = length vec)
mergesortCost :: V.Vector ElmT -> AbstCost
mergesortCost vec = let len = V.length vec in
                    len * (logBase 2 len)


parMergesort :: V.Vector ElmT -> Par (V.Vector ElmT)
parMergesort vec = if V.length vec < 2
                   then return vec
                   else do
                    let n = (V.length vec) `div` 2
                    let (lhalf, rhalf) = V.splitAt n vec
                    ileft  <- oracleSpawn (makeStableName "mergesort") lhalf
                    iright <- oracleSpawn (makeStableName "mergesort") rhalf
                    left   <- get ileft
                    right  <- get iright
                    oracleSpawn (makeStableName "merge") left right

-- Just a sample. Could change to do in-place merge.
seqMerge :: V.Vector ElmT -> V.Vector ElmT -> Par (V.Vector ElmT)
seqMerge left right = if V.null left then return right
                      else if V.null right then return left
                      else if (V.head left) < (V.head right)
                      then return (V.head left)  V.cons (seqMerge (V.tail left) right)
                      else return (V.head right) V.cons (seqMerge left (V.tail right))

seqMergeCost :: V.Vector ElmT -> V.Vector ElmT -> AbstCost
seqMergeCost left right = (V.length left) + (V.length right)


parMerge :: V.Vector ElmT -> V.Vector ElmT -> Par (V.Vector ElmT)
parMerge left right = undefined
        {-
         - Do the binary splitting thing. Interesting lines:
         -
         - ileft  <- oracleSpawn (makeStableName "merge") lleft lright
         - iright <- oracleSpawn (makeStableName "merge") rleft rright
         - left   <- get ileft
         - right  <- get iright
         - return $ left ++ right
         -}

parMergeCost :: V.Vector ElmT -> V.Vector ElmT -> AbstCost
parMergeCost left right = undefined


{- User will have to have an initialization function to register their
 - sub-implementations. For each "super function" (i.e., mergesort, merge, 
 - etc.), need to make an implList containing info on the possible sub-impls 
 - (resource to use, function definition, cost). Could optionally separate 
 - into separate initialization functions if desired.
 -} 
init :: Par ()
init = let msImplList = [(Seq, seqMergesort, mergesortCost),
                         (CPU, parMergesort, mergesortCost)]
           mergeImplList = [(Seq, seqMerge, seqMergeCost),
                            (CPU, parMerge, parMergeCost)] in
        do  mkOracleFun (makeStableName "mergesort") msImplList
            mkOracleFun (makeStableName "merge")     mergeImplList
