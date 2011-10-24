{-# LANGUAGE ScopedTypeVariables, CPP, BangPatterns #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
-- -Wall -fno-warn-name-shadowing

-- | Experimental support for 'OpenList's, which are streams in
-- the 'Par' monad that support constant-time append.

module Control.Monad.Par.OpenList
 (
  OpenList(),
  empty, singleton, cons, head, tail, length,
  close, join,
  toList, fromList, toLazyList, 
  parMapM, parBuild, parBuildM,  
  openlist_tests, 
  chaintest, 
  async_test, lazy_chaintest

-- , IList(..), newCell
 )
where 

import Control.Monad hiding (join)
import Control.DeepSeq
import Control.Concurrent.MVar
-- import Control.Monad.Par hiding (parMapM)
import Control.Monad.Par.IList
import Control.Monad.Par.Scheds.Trace 
import Control.Monad.Par.Scheds.TraceInternal
import qualified Control.Monad.Par.Combinator as C

import Prelude hiding (length,head,tail,drop,take,null)
import qualified Prelude as P
-- import System.IO.Unsafe
import GHC.IO (unsafePerformIO, unsafeDupablePerformIO)
import Test.HUnit 
import Debug.Trace

-- -----------------------------------------------------------------------------
-- Open Lists -- IVars at the tail.
--
-- These have some of the advantages of imperative lists, such as
-- constant time appending, while retaining determinism and having
-- O(1) access to the head of the list unlike tree-shaped lists
-- (e.g. append-based rather than cons-based).


-- An OpenList must be handled functionally.  Extending the list as
-- an effect will not change its tail pointer.
data OpenList a = OpenList (IList a) (IList a)

-- This is likewise a pretty meaningless NFData instance:
instance NFData a => NFData (OpenList a) where 
  rnf (OpenList hp tp) = rnf hp `seq` rnf tp 


-- | An empty open list.  Supports further extension.
empty :: OpenList a
empty = OpenList Null Null

null :: OpenList a -> Bool
null (OpenList Null Null) = True
null _                    = False

-- | A single element open list.
singleton :: a -> Par (OpenList a)
singleton x = 
  do pv <- new 
     let cell = Cons x pv
     return (OpenList cell cell)

-- TODO/FIXME: Need to decide whether there should be closed and open empty lists!!

-- | Terminate a non-empty open list so that it cannot be extended further.
close :: NFData a => OpenList a -> Par (OpenList a)
close orig@(OpenList Null _) = return orig
close orig@(OpenList _   tp) = do put (tl tp) Null; return orig


-- This version ignores the tail pointer and seeks out the end of the
-- list (at the present time).
-- unsafeClose :: NFData a => OpenList a -> Par (OpenList a)
-- unsafeClose orig@(OpenList Null _) = return orig



-- | Destructive append operation.
join :: NFData a => OpenList a -> OpenList a -> Par (OpenList a)
join (OpenList Null _) right = return right
join left  (OpenList Null _) = return left 
join (OpenList hp1 tp1) (OpenList hp2 tp2) =
    do put (tl tp1) hp2
       return (OpenList hp1 tp2)

-- | Head of an OpenList.
head :: OpenList a -> a
head (OpenList Null _) = error "cannot take head of null OpenList"
head (OpenList hp _)   = hd hp

headCell (OpenList hp _) = OpenList hp hp 
lastCell (OpenList _ tp) = OpenList tp tp

-- | Tail of an OpenList.  Beware, if the list contains only one
--   element (e.g. the result of tail will be null), it must be CLOSED
--   for tail to work.
tail :: OpenList a -> Par (OpenList a)
-- NOTE: We could fix this limitation by adding a length field to the OpenList.
tail (OpenList Null _) = error "cannot take tail of null OpenList"
tail (OpenList hp tp)  = 
  do nxt <- get (tl hp)
     case nxt of
       Null -> return empty
       _    -> return (OpenList nxt tp)


drop :: NFData a => Int -> OpenList a -> Par (OpenList a)
drop 0 ls = return ls
drop n ls = do tl <- tail ls
	       drop (n-1) tl

-- This copies a prefix and makes it open once again irrespective of
-- whether the input list is open or closed.
take :: NFData a => Int -> OpenList a -> Par (OpenList a)
take 0 ls = return empty
take n ls = do tl   <- tail ls
	       rest <- take (n-1) tl
	       cons (head ls) rest

-- Take the length of a closed OpenList.
-- length :: OpenList a -> Par Int
length (OpenList Null _) = return 0
-- length (OpenList (Cons a _) (Cons b _)) | a == b = return 1
length ls = do t   <- tail ls 
	       len <- length t
	       return (len+1)

-- | Add an element to the front of an OpenList.  Works irrespective
-- | of whether the input is closed.
cons :: NFData a => a -> OpenList a -> Par (OpenList a)
-- Careful, consing should not close the openlist:
cons car (OpenList Null _) = singleton car
cons car (OpenList hp tp) = 
  do cdr <- newFull_ hp
     return (OpenList (Cons car cdr) tp)

newCell x = do pv <-new; return (Cons x pv)

-- | Convert a list to an OpenList, open to extension at the tail.
fromList :: NFData a => [a] -> Par (OpenList a)
fromList [] = return empty
fromList (h:t) = 
  --   This function is inefficient and could be replaced with an additional IList data constructor.
    do head <- newCell h
       rest <- loop head t
       return (OpenList head rest)
 where 
   loop last  []   = return last
   loop last (h:t) = 
    do cell <- newCell h
       put (tl last) cell
       loop cell t

-- | Convert a CLOSED OpenList to a list. 
toList :: NFData a => (OpenList a) -> Par [a] 
-- Note: presently not tail-recursive:
toList (OpenList hp _) = loop hp
 where 
  loop Null = return []
  loop (Cons head pv) = 
    do 
       rest <- get pv
       converted <- loop rest
       return (head : converted)

-- Strict map over closed lists.
-- 
-- parMap for OpenLists does not support a nice divide-and-conquer.
-- (Well, it would support the appending if we were willing to scan ahead to find the length.)
-- OpenLists are not Traversable... so we can't just use Par.parMap.
--
-- TODO: Perhaps this should use a strategy for each element:
-- parMapM :: NFData b => (a -> Par b) -> OpenList a -> Par (OpenList b)
parMapM _ (OpenList Null _) = return empty
-- parMapM fn (OpenList (Cons a _) (Cons b _)) | a == b =  fn a >>= singleton 
parMapM fn ls = 
  do h <- spawn (fn (head ls))
     t <- tail ls
     h' <- get h
     t2 <- parMapM fn t
     cons h' t2

-- maybe the following too?
-- parMapM_ :: (a -> Par ()) -> OpenList a -> Par () 

-- | Build an OpenList with a divide-and-conquer parallel strategy.
parBuild :: NFData a => C.InclusiveRange -> (Int -> a) -> Par (OpenList a)
parBuild range fn =
  C.parMapReduceRange range (singleton . fn) join empty

-- | Build an OpenList with a divide-and-conquer parallel strategy,
--   allowing nested parallelism in the per-element computation.
parBuildM :: NFData a => C.InclusiveRange -> (Int -> Par a) -> Par (OpenList a)
parBuildM range fn =
  C.parMapReduceRange range ((>>= singleton) . fn) join empty


-- | OpenLists can only be printed properly in the Par monad.  @show@
--   on an open list will only give a hint -- what the first and last
--   elements of the openlist are.
instance Show a => Show (OpenList a) where 
  show (OpenList Null _) = "OpenList []"
  show (OpenList (Cons fst _) (Cons lst _)) = 
      "OpenList ["++show fst++".."++ show lst ++"]"

       

debugshow (OpenList (Cons h1 _) (Cons h2 _)) = "Cons|Cons|eq/"++show(h1==h2)
debugshow (OpenList Null       Null)       = "Null|Null"
debugshow (OpenList Null       (Cons _ _)) = error$ "invalid Null|Cons openlist"
debugshow (OpenList (Cons _ _) Null)       = error$ "invalid Cons|Null openlist"

-- Check the length of an openlist from head pointer to tail pointer
-- (not including anything present beyond the tail pointer).
-- WARNING: ASSUMES UNIQUE ELEMENTS:
debuglength :: Eq a => OpenList a -> Par Int
debuglength (OpenList Null Null) = return 0
debuglength orig@(OpenList (Cons hp1 tp1) (Cons hp2 tp2))
  | hp1 == hp2 = return 1
  | otherwise  = do rest <- tail orig
    	  	    sum  <- debuglength rest
		    return (1 + sum)

-- -----------------------------------------------------------------------------
-- Synchronization using native Haskell IVars (e.g. MVars).

-- The MList datatype is internal to the module.
-- These MVars are only written once:
data MList a = MNull | MCons (a, MVar (MList a))

_unsafe_io :: IO a -> Par a
_unsafe_io io =  let x = unsafePerformIO io in
		 x `seq` return x

_unsafe_dupable :: IO a -> Par a
_unsafe_dupable io = 
  let x = unsafeDupablePerformIO io in 
  x `seq` return x

-- Return a lazy list:
mListToList :: MList a -> [a]
mListToList MNull = []
mListToList (MCons(hd,tl)) = 
    let rest = unsafeDupablePerformIO$ 
	       do tl' <- readMVar tl  
		  return (mListToList tl')
    in  (hd : rest)

iListToMList :: IList a -> Par (MList a)
iListToMList Null = return MNull
iListToMList il = 
  do mv <- _unsafe_dupable newEmptyMVar
     fork $ do t <- get (tl il)
	       r <- iListToMList t
	       _unsafe_io$ putMVar mv r
     return (MCons (hd il, mv))

-- | Asynchronously convert an OpenList to a lazy list.  Returns immediately.
toLazyList :: OpenList a -> Par [a]
toLazyList (OpenList head _) = iListToMList head >>= return . mListToList 
-- toLazyList ol = toMList ol >>= return . mListToList 






-- -----------------------------------------------------------------------------
-- Testing
-- -----------------------------------------------------------------------------

test_ol0 = runPar (cons 'a' empty >>= cons 'b' >>= close >>= tail >>= tail >>= length)

test_ol1 :: Int
test_ol1 = runPar$ do l :: OpenList Int <- join empty empty
		      length l

test_ol2 :: String
test_ol2 = show$ runPar$ do 
 ls1 <- fromList [10,11,12]
 ls2 <- singleton (5::Int)
 join ls1 ls2

test_ol3 :: [Int]
test_ol3 = runPar$ do ol :: OpenList Int <- fromList [1..10]
		      close ol
		      toList ol

test_ol4 :: Int
test_ol4 = runPar$ do ol <- fromList [1..10]		      
		      t1 <- tail ol
		      t2 <- tail t1
		      return (head t2)

test_ol5 :: Int
test_ol5 = runPar$ fromList ([1..10] :: [Int]) >>= close >>= length

test_ol6 :: [Int]
test_ol6 = runPar$ do
  l1 <- fromList [1..10]
  close l1
  l2 <- parMapM (return . (+ 1)) l1
  close l2
  toList l2


test_ol7 :: [Int]
test_ol7 = runPar$ do 
  a <- singleton 1
  b <- singleton 2
  join a b
  close b
  toLazyList a

test_ol8 :: [Int]
test_ol8 = runPar$ do 
  a <- singleton 1
  b <- singleton 2
  c <- singleton 3
  d <- singleton 4
  join c d
  join a b
  join b c
  close d
  toLazyList a

test_ll :: [Int]
test_ll = runPar$
   do l <- fromList [1..1000]
      close l 
      toLazyList l

chaintest :: Int -> Par (IList Int)
chaintest 0   = error "must have chain length >= 1"
chaintest len = loop 0 len 
 where 
   loop i 1 = do tl <- if i == len-1 
		       then newFull_ Null 
		       else new
                 when (i == len-1) (print_$ " == GOT END: "++show i)
		 return (Cons i tl)
   loop i n =
    do let half = n `quot` 2
       ifst <- spawn_$ loop i half 
       fork $ do 
		 snd <- loop (i+half) half
		 fst <- get ifst
		 lastfst <- dropIList (half-1) fst 
		 put (tl lastfst) snd
		 return ()     
       get ifst

dropIList :: NFData a => Int -> IList a -> Par (IList a)
dropIList 0 ls = return ls
dropIList n ls = do rest <- get (tl ls)
	            dropIList (n-1) rest

-- lazy_chaintest i = chaintest i >>= toLazyList
lazy_chaintest :: Int -> Par [Int]
lazy_chaintest i = do il <- chaintest i 
		      ml <- iListToMList il
		      return (mListToList ml)

-- If we create a large, lazy chain, taking just the head should be quick.
async_test = 
  do putStrLn "BeginTest"
--     let lazy = runParAsync$ lazy_chaintest 1048576
--     let lazy = runParAsync$ lazy_chaintest 32768
     let lazy = runParAsync$ lazy_chaintest 1024
--     let lazy = runPar$ lazy_chaintest 1024
     putStrLn$ "Resulting list "++ show lazy
     putStrLn$ "Got head: "++ show (P.take 3 lazy)
     putStrLn "EndTest"

--------------------------------------------------------------------------------

print_ msg = trace msg $ return ()

dbg0 = debugshow$ runPar$ singleton 'a' >>= close 

-- This one is an error:
err1 = debugshow$ runPar$ singleton 'a' >>= tail  >>= close 
-- But this will work:
dbg1 = debugshow$ runPar$ fromList "ab" >>= tail  >>= close 

dbg2 = debugshow$ runPar$ singleton 'a' >>= close >>= tail 

-- This is invalid:
err2 = debugshow$ runPar$ singleton 'a' >>= tail 

-- TODO: HUnit These.
openlist_tests :: Test
openlist_tests = 
  TestList 
    [
     -- First a few small, unnamed tests:
     0                   ~=? runPar (length (empty :: OpenList Int)),
     "a"                 ~=? runPar (singleton 'a' >>= close >>= toList),
     1                   ~=? runPar (singleton 'a' >>= close >>= length),
     1                   ~=? runPar (cons 'b' empty >>= close >>= length),

     TestLabel "singleton, close"          $ "Cons|Cons|eq/True" ~=? dbg0,
     TestLabel "tail then close - SKETCHY" $ "Cons|Cons|eq/True" ~=? dbg1,
     TestLabel "close then tail"           $ "Null|Null" ~=? dbg2,
--     TestLabel "tail no close"   $ "" ~=? dbg3,

     TestLabel "tail of singleton" $ 
     0                   ~=? runPar (singleton 'a' >>= close >>= tail >>= length),
     TestLabel "tail tail of cons cons" $ 
     0                   ~=? test_ol0,

     TestLabel "join of two empty's still length zero" $ 
     0                   ~=? test_ol1,
     TestLabel "test show instance" $ 
     "OpenList [10..5]"  ~=? test_ol2,

     TestLabel "toList" $ 
     [1..10]             ~=? test_ol3,

     TestLabel "head . tail . tail" $ 
     3                   ~=? test_ol4,

     TestLabel "length . fromList" $ 
     10                  ~=? test_ol5,

     TestLabel "test parMap" $ 
     [2..11]             ~=? test_ol6,

     TestLabel "test 7" $ 
     [1,2]              ~=? test_ol7,

     TestLabel "test 8" $ 
     [1..4]             ~=? test_ol8,

     TestLabel "test lazy list conversion" $ 
     [1..1000]          ~=? test_ll,

     TestLabel "chaintest" $
     [0..511]           ~=? runPar (lazy_chaintest 512),

     TestLabel "asynchronous chaintest" $
     [0..511]           ~=? runParAsync (lazy_chaintest 512)

    ]

