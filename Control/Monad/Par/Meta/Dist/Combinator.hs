{-# LANGUAGE BangPatterns, CPP, TemplateHaskell, ScopedTypeVariables #-} 
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
    InclusiveRange(..),
--    parFor
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

import Remote2.Closure  (Closure(Closure))
import Remote2.Encoding (Payload, Serializable, serialDecodePure, getPayloadContent, getPayloadType)
import qualified Remote2.Reg as Reg

#else 
#endif
import Control.Monad.Par.Class



-- -- {-# SPECIALISE parMap  :: (NFData b) => (a -> b)     -> [a] -> PT.Par [b] #-}
-- -- {-# SPECIALISE parMap  :: (NFData b) => (a -> b)     -> [a] -> PD.Par [b] #-}
-- -- {-# SPECIALISE parMapM :: (NFData b) => (a -> Par b) -> [a] -> PT.Par [b] #-}
-- -- {-# SPECIALISE parMapM :: (NFData b) => (a -> Par b) -> [a] -> PD.Par [b] #-}

-- -- -----------------------------------------------------------------------------
-- -- Parallel maps over Traversable data structures

-- -- | Applies the given function to each element of a data structure
-- -- in parallel (fully evaluating the results), and returns a new data
-- -- structure containing the results.
-- --
-- -- > parMap f xs = mapM (spawnP . f) xs >>= mapM get
-- --
-- -- @parMap@ is commonly used for lists, where it has this specialised type:
-- --
-- -- > parMap :: NFData b => (a -> b) -> [a] -> Par [b]
-- --
-- parMap :: (Traversable t, NFData b, ParFuture p iv) => (a -> b) -> t a -> p (t b)
-- parMap f xs = mapM (spawnP . f) xs >>= mapM get

-- -- | Like 'parMap', but the function is a @Par@ monad operation.
-- --
-- -- > parMapM f xs = mapM (spawn . f) xs >>= mapM get
-- --
-- parMapM :: (Traversable t, NFData b, ParFuture p iv) => (a -> p b) -> t a -> p (t b)
-- parMapM f xs = mapM (spawn . f) xs >>= mapM get

-- -- TODO: parBuffer



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
-- > parMapReduceRangeThresh 100 (InclusiveRange 1 (10^6))
-- >        (\x -> return (x^2))
-- >        (\x y -> return (x+y))
-- >        0
--
{-
parMapReduceRangeThresh
   :: (NFData a, ParFuture p iv)
      => Int                            -- ^ threshold
      -> InclusiveRange                 -- ^ range over which to calculate
      -> (Int -> p a)                 -- ^ compute one result
      -> (a -> a -> p a)              -- ^ combine two results (associative)
      -> a                              -- ^ initial result
      -> p a

parMapReduceRangeThresh threshold (InclusiveRange min max) fn binop init
 = loop min max
 where
  loop min max
    | max - min <= threshold =
	let mapred a b = do x <- fn b;
			    result <- a `binop` x
			    return result
	in foldM mapred init [min..max]

    | otherwise  = do
	let mid = min + ((max - min) `quot` 2)
	rght <- spawn $ loop (mid+1) max
	l  <- loop  min    mid
	r  <- D.get rght
	l `binop` r
-}

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
type Mapper  a = Closure (Int -> D.Par a)
type Reducer a = Closure (a -> a -> D.Par a)

parMapReduceRange :: -- (NFData a, Serialize a) => 
--		     InclusiveRange -> TH.Name -> TH.Name -> a -> D.Par a
--		     InclusiveRange -> Mapper a -> Reducer a -> a -> D.Par a
		     InclusiveRange -> Mapper Payload -> Reducer Payload -> Payload -> D.Par Payload
-- parMapReduceRange :: (NFData a, ParFuture p iv) => 
-- 		     InclusiveRange ->  (Int -> p a) -> (a -> a -> p a) -> a -> p a
parMapReduceRange (InclusiveRange start end) map_clo binop_clo init =
   pmrr_loop(length segs, segs, init, map_clo, binop_clo)
 where
  segs = splitInclusiveRange (auto_partition_factor * numCapabilities) (start,end)


--pmrr_loop :: (Int, [(Int,Int)], TH.Name, TH.Name) -> D.Par BS.ByteString
-- pmrr_loop :: (Int, [(Int,Int)], Mapper a, Reducer a) -> D.Par a
pmrr_loop :: (Int, [(Int,Int)], Payload, Mapper Payload, Reducer Payload) -> D.Par Payload
-- pmrr_loop( 1, [(st,en)], map_clo, binop_clo) =
pmrr_loop( n, segs, init, mpr@(Closure mapperID _), rdcr@(Closure reducerID _)) =
  case segs of 
   [(st,en)] -> 
     -- Execute serially:
     let mapred a b = do 
                         x :: Payload <- appMapper b;
			 appBinop a x :: D.Par Payload

     in foldM mapred init [st..en]

-- pmrr_loop( n, segs, fn_name, binop_name) =
   segs -> 
     let half = n `quot` 2
	 (left,right) = splitAt half segs in
     do 
#ifdef DIST
        l  <- D.longSpawn$ $(mkClosureRec 'pmrr_loop)  (half, left, init, mpr, rdcr)
#else
        l  <- spawn_$ pmrr_loop (half, left, init, mpr, rdcr)
#endif
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


-- -- TODO: A version that works for any splittable input domain.  In this case
-- -- the "threshold" is a predicate on inputs.
-- -- parMapReduceRangeGeneric :: (inp -> Bool) -> (inp -> Maybe (inp,inp)) -> inp ->


-- -- Experimental:

-- -- | Parallel for-loop over an inclusive range.  Semantically equivalent
-- -- to
-- -- 
-- -- > parFor (InclusiveRange n m) f = forM_ [n..m] f
-- --
-- -- except that the implementation will split the work into an
-- -- unspecified number of subtasks in an attempt to gain parallelism.
-- -- The exact number of subtasks is chosen at runtime, and is probably
-- -- a small multiple of the available number of processors.
-- --
-- -- Strictly speaking the semantics of 'parFor' depends on the
-- -- number of processors, and its behaviour is therefore not
-- -- deterministic.  However, a good rule of thumb is to not have any
-- -- interdependencies between the elements; if this rule is followed
-- -- then @parFor@ has deterministic semantics.  One easy way to follow
-- -- this rule is to only use 'put' or 'put_' in @f@, never 'get'.

-- parFor :: (ParFuture p iv) => InclusiveRange -> (Int -> p ()) -> p ()
-- parFor (InclusiveRange start end) body =
--  do
--     let run (x,y) = for_ x (y+1) body
--         range_segments = splitInclusiveRange (4*numCapabilities) (start,end)

--     vars <- M.forM range_segments (\ pr -> spawn_ (run pr))
--     M.mapM_ get vars
--     return ()


-- -- My own forM for numeric ranges (not requiring deforestation optimizations).
-- -- Inclusive start, exclusive end.
-- {-# INLINE for_ #-}
-- for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
-- for_ start end _fn | start > end = error "for_: start is greater than end"
-- for_ start end fn = loop start
--   where
--    loop !i | i == end  = return ()
-- 	   | otherwise = do fn i; loop (i+1)
