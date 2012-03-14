{-# LANGUAGE BangPatterns, CPP, TemplateHaskell, ScopedTypeVariables #-} 

-- UNFINISHED UNFINISHED UNFINISHED UNFINISHED UNFINISHED UNFINISHED 

{-| 
    A collection of useful parallel combinators based on top of a 'Par' monad.

    In particular, this module provides higher order functions for
     traversing data structures in parallel.  

-}

module Control.Monad.Par.Meta.Dist.Combinator
  (
--    parMap, parMapM,
--    parMapReduceRangeThresh, 
    parMapReduceRange,
    InclusiveRange(..)
--    parFor
--    Mapper, Reducer
  )
where 

import Control.DeepSeq
import Data.Traversable
import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import GHC.Conc (numCapabilities)

import qualified Control.Monad.Par.Scheds.Trace as T
import qualified Data.ByteString.Char8 as BS

#define DIST

#ifdef DIST
import Data.IORef (readIORef)
import Control.Monad.IO.Class (liftIO)
import Data.Serialize
import qualified Language.Haskell.TH as TH
import qualified Control.Monad.Par.Meta.Dist as D
-- Tweaked version of CloudHaskell's closures:
import Remote2.Call (mkClosureRec, remotable)

import Remote2.Closure  (Closure(Closure), BiClosure)
import Remote2.Encoding (Payload, Serializable, serialDecodePure, getPayloadContent, getPayloadType)
import qualified Remote2.Reg as Reg

#else 
#endif
import Control.Monad.Par.Class



----------------------------------------------------------------------------------------------------

-- -- TODO: Perhaps should introduce a class for the "splittable range" concept.
data InclusiveRange = InclusiveRange Int Int


splitInclusiveRange :: Int -> (Int, Int) -> [(Int, Int)]
splitInclusiveRange pieces (start,end) =
  map largepiece [0..remain-1] ++
  map smallpiece [remain..pieces-1]
 where
   len = end - start + 1 -- inclusive [start,end]
   (portion, remain) = len `quotRem` pieces
   largepiece i =
       let offset = start + (i * (portion + 1))
       in (offset, offset + portion)
   smallpiece i =
       let offset = start + (i * portion) + remain
       in (offset, offset + portion - 1)

----------------------------------------------------------------------------------------------------


-- How many tasks per process should we aim for?  Higher numbers
-- improve load balance but put more pressure on the scheduler.
auto_partition_factor :: Int
auto_partition_factor = 4

-- | \"Auto-partitioning\" version of 'parMapReduceRangeThresh' that chooses the threshold based on
--    the size of the range and the number of processors..
--
--   The limitation of this approach in the distributed context is
--   that the mapper and reducer functions have to operate 

type FunId = String

-- type Mapper = Int -> Closure a
-- type Reducer = ((a, a) -> Closure a)
type Mapper  a = BiClosure (Int -> D.Par a)
type Reducer a = BiClosure (a -> a -> D.Par a)

type Mapper1  a = Closure (Int -> D.Par a)
type Reducer1 a = Closure (a -> a -> D.Par a)


-- | Computes a binary map\/reduce over a finite range.  The range is
-- recursively split into two, the result for each half is computed in
-- parallel, and then the two results are combined.  When the range
-- reaches the threshold size, the remaining elements of the range are
-- computed sequentially.
--
-- For example, the following is a parallel implementation of
--
-- >  foldl (+) 0 (map (^2) [1..10^6])
--
-- > parMapReduceRange (InclusiveRange 1 (10^6))
-- >        (\x -> return (x^2))
-- >        (\x y -> return (x+y))
-- >        0
--
--
-- #define POLY 
#ifdef POLY
parMapReduceRange :: (NFData a, Serialize a) => 
		     InclusiveRange -> Mapper a -> Reducer a -> a -> D.Par a
#else
parMapReduceRange :: InclusiveRange -> Mapper Payload -> Reducer Payload -> Payload -> D.Par Payload
#endif
parMapReduceRange (InclusiveRange start end) (_,map_clo) (_,binop_clo) init =
   pmrr_loop(length segs, segs, init, map_clo, binop_clo)
 where
  segs = splitInclusiveRange (auto_partition_factor * numCapabilities) (start,end)


#ifdef POLY
--pmrr_loop :: forall a . (NFData a, Serialize a) 
pmrr_loop :: (NFData a, Serialize a) 
          => (Int, [(Int,Int)], a, Mapper a, Reducer a) -> D.Par a
#else
pmrr_loop :: (Int, [(Int,Int)], Payload, Mapper1 Payload, Reducer1 Payload) -> D.Par Payload
#endif
pmrr_loop( n, segs, init, mpr@(Closure mapperID _), rdcr@(Closure reducerID _)) =
  case segs of 
   [(st,en)] -> 
     -- Execute serially:
     let mapred a b = do 
#ifdef POLY
                         x :: a <- appMapper b;
			 appBinop a x :: D.Par a
#else
                         x :: Payload <- appMapper b;
			 appBinop a x
#endif 
     in foldM mapred init [st..en]

   segs -> 
     let half = n `quot` 2
	 (left,right) = splitAt half segs in
     do 
        l  <- D.longSpawn$ $(mkClosureRec 'pmrr_loop)  
	                   (half, left, init, mpr, rdcr)
        r  <- pmrr_loop (n-half, right, init, mpr, rdcr)
	l' <- D.get l
	appBinop l' r

  where 
   -- Here we have to do closure lookups:
   appMapper x = do
     lkp <- liftIO$ readIORef D.globalRPCMetadata
     case Reg.getEntryByIdent lkp mapperID of 
       Nothing -> fail$ "parMapReduceRange: failed to deserialize closure identifier: "++show mapperID
       Just fn -> return (fn x)

   appBinop x y = do
     lkp <- liftIO$ readIORef D.globalRPCMetadata
     case Reg.getEntryByIdent lkp reducerID of 
       Nothing -> fail$ "parMapReduceRange: failed to deserialize closure identifier: "++show reducerID
       Just fn -> return (fn x y)

instance NFData Payload where 
  rnf _ = ()

-- Generate stub code for RPC:
remotable ['pmrr_loop]

