{-# LANGUAGE CPP #-}
-- -*- haskell -*-
--
-- ADT of a binary tree (values only in leaves).
-- Parallel functions use par and seq directly.
-- ---------------------------------------------------------------------------

module Tree(Tree, 
            list2tree, tree2list, (^:), 
            tree_map, tree_fold, 
	    depth, create_forest, 
            force_tree, par_tree_map) where

import Control.Parallel
import Control.Parallel.Strategies
#ifdef PARSCHED 
import PARSCHED
#else
import Control.Monad.Par
#endif

infixl 2 ^:

data Tree a = Leaf a
	    | Node (Tree a) (Tree a)
	    deriving (Eq, Read, Show)

		
tree_map :: (Integral a, Integral b) => (a -> b) -> Tree a -> Tree b
tree_map f (Leaf x) 		= Leaf (f x)
tree_map f (Node left right) 	= Node (tree_map f left) (tree_map f right)


par_tree_map :: (Integral a, Integral b) => (a -> b) -> Tree a -> Tree b
par_tree_map f t =
    runPar $ par_tree_map' f t

par_tree_map' :: (Integral a, Integral b) => (a -> b) -> Tree a -> Par (Tree b)
par_tree_map' f (Leaf x) = return $ Leaf (f x)
par_tree_map' f (Node left right) = 
    do l' <- spawn_ $ par_tree_map' f left
       r  <- par_tree_map' f right
       l  <- get l'
       return (Node l r)

-- force evaluation of tree (could use Strategies module instead!)
force_tree :: (Integral a) => Tree a -> ()
force_tree t@(Leaf x) = x `seq` ()
force_tree t@(Node left right) = (force_tree left) `seq` 
	                         (force_tree right) `seq` 
				 ()
-- just would you'd expect
tree_fold :: (Integral a) => (a -> a -> a) -> a -> Tree a -> a
tree_fold o z (Leaf x) 		= z `o` x
tree_fold o z (Node left right) = tree_fold o z' right
				  where z' = tree_fold o z left

list2tree :: (Integral a) => [a] -> Tree a 
list2tree [] 	= error "list2tree: empty list"
list2tree [x] 	= Leaf x
list2tree l     = Node (list2tree left) (list2tree right)
		  where (left,right) = splitAt ((length l) `div` 2 ) l

tree2list :: (Integral a) => Tree a -> [a]
tree2list (Leaf x) 	= [x]
tree2list (Node left right) = tree2list left ++ tree2list right

-- combine 2 trees
(^:) :: (Integral a) => Tree a -> Tree a -> Tree a
t1 ^: t2 = Node t1 t2

depth :: Tree a -> Int
depth (Leaf _)		= 0
depth (Node left right) = max (depth left) (depth right) + 1

-- The following functions are useful for heavily heap allocating test fcts
create_forest :: (Integral a) => Tree a -> [Tree a] 
create_forest (Leaf x) 		= [ (Leaf y) | y <- [2..x], gcd x y == 1 ]
create_forest (Node left right) = [ (Node left' right') 
				  | left' <- create_forest left,
				    right' <- create_forest right]

{-
On a Harpertown Windows machine with 4 cores (8 threads), using
Haskell Platform 2011.2.0.0. 
Note the CPU utilization rates (elapsed time * num_of_threads/CPU time)
range between 70% and 77% when -N > 1. 

Compare that with Strategies, which has a higher CPU utilization rate
(> 95%) and a smaller user time for -N > 1.

E:\cchen15\icfp\partree-mp>timeit partree.exe 1000 10 +RTS -N1
partree 1000 10 = 23712

Fri Mar 18 13:56:45 2011
Cmd: partree.exe 1000 10 +RTS -N1
Elapsed:   4.898 sec
User       4.867 sec
System     0.016 sec
Total CPU  4.883 sec
Peak WorkingSet Size:     7229440
Peak Pagefile Usage:      5836800
Page Fault Count:         1806
Peak Paged Pool usage:    85688
Peak NonPaged Pool usage: 5568

E:\cchen15\icfp\partree-mp>timeit partree.exe 1000 10 +RTS -N2
partree 1000 10 = 23712

Fri Mar 18 13:56:54 2011
Cmd: partree.exe 1000 10 +RTS -N2
Elapsed:   9.313 sec
User       12.605 sec
System     1.841 sec
Total CPU  14.446 sec
Peak WorkingSet Size:     12619776
Peak Pagefile Usage:      10133504
Page Fault Count:         3122
Peak Paged Pool usage:    85688
Peak NonPaged Pool usage: 6048

E:\cchen15\icfp\partree-mp>timeit partree.exe 1000 10 +RTS -N4
partree 1000 10 = 23712

Fri Mar 18 13:57:07 2011
Cmd: partree.exe 1000 10 +RTS -N4
Elapsed:   9.968 sec
User       22.761 sec
System     5.756 sec
Total CPU  28.517 sec
Peak WorkingSet Size:     13414400
Peak Pagefile Usage:      11677696
Page Fault Count:         3317
Peak Paged Pool usage:    85688
Peak NonPaged Pool usage: 6720

E:\cchen15\icfp\partree-mp>timeit partree.exe 1000 10 +RTS -N8
partree 1000 10 = 23712

Fri Mar 18 13:57:25 2011
Cmd: partree.exe 1000 10 +RTS -N8
Elapsed:   9.843 sec
User       46.239 sec
System     11.029 sec
Total CPU  57.268 sec
Peak WorkingSet Size:     14729216
Peak Pagefile Usage:      15175680
Page Fault Count:         3637
Peak Paged Pool usage:    85688
Peak NonPaged Pool usage: 7968
-}