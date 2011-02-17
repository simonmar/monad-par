{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fwarn-unused-imports #-}

module Control.Monad.Par.OpenList 
 -- (
 --  OpenList(),
 --  empty, singleton, cons, head, tail, length,
 --  close, join,
 --  toList, fromList,
 --  parMapM, parBuild, parBuildM,
 --  openlist_tests
 -- )
where 


import Control.DeepSeq
import Control.Monad.Par hiding (parMapM)
import Prelude hiding (length,head,tail)
import Test.HUnit 
import Debug.Trace

-- -----------------------------------------------------------------------------
-- Open Lists -- PVars at the tail.
-- TODO: FACTOR INTO SEPARATE FILE.

-- These have some of the advantages of imperative lists, such as
-- constant time appending, while retaining determinism and having
-- O(1) access to the head of the list unlike tree-based lists (e.g. append
-- rather than cons-based).

data IList a = Null | Cons { hd :: a, tl :: PVar (IList a) }

data OpenList a = OpenList (IList a) (IList a)

-- | To fully evaluate an open list means to evaluate all of the
--   available car fields.  There is nothing to be done about the fact
--   that the trailing PVar cdr field may receive further extensions.
instance NFData a => NFData (IList a) where 
--  rnf Null = r0
  rnf Null = ()
  rnf (Cons a b) = rnf a `seq` rnf b

-- | An empty open list.
empty :: OpenList a
empty = OpenList Null Null

-- | A single element open list.
singleton :: a -> Par (OpenList a)
singleton x = 
  do pv <- new 
     let cell = Cons x pv
     return (OpenList cell cell)

-- | Terminate an open list so that it cannot be extended further.
close :: NFData a => OpenList a -> Par (OpenList a)
close orig@(OpenList Null _) = return orig
close orig@(OpenList _   tp) = do put (tl tp) Null; return orig

-- | Destructive append operation.
join :: NFData a => OpenList a -> OpenList a -> Par (OpenList a)
join (OpenList Null _) right = return right
join left  (OpenList Null _) = return left 
join (OpenList hp1 tp1) (OpenList hp2 tp2) =
    do put (tl tp1) hp2
       return (OpenList hp1 tp2)

head :: OpenList a -> a
head (OpenList Null _) = error "cannot take head of null OpenList"
head (OpenList hp _)   = hd hp

tail :: OpenList a -> Par (OpenList a)
tail (OpenList Null _) = error "cannot take tail of null OpenList"
tail (OpenList hp tp)  = 
  do nxt <- get (tl hp)
     return (OpenList nxt tp)

-- Take the length of a closed OpenList.
-- length :: OpenList a -> Par Int
length (OpenList Null _) = return 0

-- length (OpenList (Cons a _) (Cons b _)) | a == b = error "length Last cell."
length (OpenList (Cons a _) (Cons b _)) | a == b = return 1

length ls = do t   <- tail ls 
	       trace (" got tail, doing length... ") $ return ()
	       len <- length t
	       trace (" got length, incrementing... "++ show len) $ return ()
	       return (len+1)

cons :: NFData a => a -> OpenList a -> Par (OpenList a)
-- Careful, consing should not close the openlist:
cons car (OpenList Null _) = singleton car
cons car (OpenList hp tp) = 
  do cdr <- newFilled hp
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

-- parMap for OpenLists does not support a nice divide-and-conquer.
-- (Well, it would support the appending if we were willing to scan ahead to find the length.)
-- OpenLists are not Traversable... so we can't just use Par.parMap.
--
-- TODO: Perhaps this should use a strategy for each element:
-- parMapM :: NFData b => (a -> Par b) -> OpenList a -> Par (OpenList b)
parMapM _ (OpenList Null _) = trace "====base case"$return empty

-- parMapM fn (OpenList (Cons a _) (Cons b _)) | a == b = error "Last cell."

parMapM fn (OpenList (Cons a _) (Cons b _)) | a == b =  fn a >>= singleton 

parMapM fn ls = 
  do h <- spawn (fn (head ls))
     t <- tail ls
--     len <- length t
--     trace ("  length of tail "++ show len) $ return ()

     h' <- trace ("getting head result..."++ show (head ls)) $ get h
--     let h' = (head ls)     
     t2 <- trace (" parmap on tail ..." ++ show t)$ parMapM fn t
     trace "debug2" $ return ()
     trace ("Consing.. HEAD is "++ show h') $ 
      cons h' t2

-- maybe the following too?
-- parMapM_ :: (a -> Par ()) -> OpenList a -> Par () 

-- | Build an OpenList with a divide-and-conquer parallel strategy.
parBuild :: NFData a => Int -> Int -> Int -> (Int -> a) -> Par (OpenList a)
parBuild threshold min max fn =
  parMapReduceRange threshold min max (singleton . fn) join empty

-- | Build an OpenList with a divide-and-conquer parallel strategy,
--   allowing nested parallelism in the per-element computation.
parBuildM :: NFData a => Int -> Int -> Int -> (Int -> Par a) -> Par (OpenList a)
parBuildM threshold min max fn =
  parMapReduceRange threshold min max ((>>= singleton) . fn) join empty


-- | OpenLists can only be printed properly in the Par monad.  @show@
--   on an open list will only give a hint -- what the first and last
--   elements of the openlist are.
instance Show a => Show (OpenList a) where 
  show (OpenList Null _) = "OpenList []"
  show (OpenList (Cons fst _) (Cons lst _)) = 
      "OpenList ["++show fst++".."++ show lst ++"]"
       

debugshow (OpenList (Cons _ _) (Cons _ _)) = "Cons|Cons"
debugshow (OpenList Null       Null)       = "Null|Null"
debugshow (OpenList Null       (Cons _ _)) = error$ "invalid Null|Cons openlist"
debugshow (OpenList (Cons _ _) Null)       = error$ "invalid Cons|Null openlist"

-- -----------------------------------------------------------------------------
-- Testing

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
test_ol5 = runPar$ fromList ([1..10] :: [Int]) >>= length

test_ol6 :: [Int]
test_ol6 = runPar$ do
  l1 <- fromList [1..10]
  l2 <- parMapM (return . (+ 1)) l1
  close l2
  toList l2

dbg0 = debugshow$ runPar$ singleton 'a' >>= close 
dbg1 = debugshow$ runPar$ singleton 'a' >>= tail  >>= close 
dbg2 = debugshow$ runPar$ singleton 'a' >>= close >>= tail 
dbg3 = debugshow$ runPar$ singleton 'a' >>= tail 

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

     TestLabel "singleton, close"$ "Cons|Cons" ~=? dbg0,
     TestLabel "tail then close" $ "" ~=? dbg1,
     TestLabel "close then tail" $ "" ~=? dbg2,
     TestLabel "tail no close"   $ "" ~=? dbg3,

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
     10                  ~=? test_ol5

--     TestLabel "" $ 
--     [2..11]             ~=? test_ol6
    ]
