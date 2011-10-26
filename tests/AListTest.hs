
module AListTest ( tests ) where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import Control.Monad.Par.AList as A
import Prelude hiding (tail, length, map, filter)
import qualified  Prelude as P



--------------------------------------------------------------------------------
-- QuickCheck properties

prop_tofrom :: [Int] -> Bool 
prop_tofrom xs = toList (fromList xs) == xs

prop_tofromB :: [Int] -> Bool 
prop_tofromB xs = toList (fromListBalanced xs) == xs

prop_balance :: [Int] -> Bool
prop_balance xs = toList (balance (fromList xs)) == xs

prop_map :: [Int] -> Bool
prop_map ls =  map (+1) (fromList ls) == fromList (P.map (+1) ls)

prop_filter :: [Int] -> Bool
prop_filter ls =  filter odd (fromList ls) == fromList (P.filter odd ls)

-- | All QuickCheck tests together:
test = mapM_ quickCheck [ prop_tofrom 
                        , prop_tofromB
                        , prop_balance
                        , prop_map
                        , prop_filter
                        ]

--------------------------------------------------------------------------------
-- Testing Utils:

bintree 0 x = x
bintree n x = Append sub sub
 where sub = bintree (n-1) x

showDbg ANil         = "_"
showDbg (ASing x)    = show x
showDbg (Append l r) = "("++showDbg l++" | "++showDbg r++")"
showDbg (AList  l)   = show l

--------------------------------------------------------------------------------

tests = [
  -- testGroup "AList HUnit Tests" (hUnitTestToTests alist_tests),

  testGroup "AList HUnit Tests" [
     testCase "fromList1"  $  8   @=? (length$ tail$ tail$ fromList [1..10])
   , testCase "cons X3"    $  1   @=? (length$ tail$tail$  cons 1$ cons 2$ cons 3 empty)
   , testCase "tail X3"    $  253 @=? (length$ tail$tail$tail$ bintree 8 $ singleton 'a')
   , testCase "len bintree"$  0   @=? (length$ bintree 8 $ empty)

   , testCase "inspect tree1"$  "((1 | 1) | (1 | 1))" @=? (showDbg$            bintree 2 $ singleton 1)
   , testCase "inspect tree2"$  "((_ | 1) | (1 | 1))" @=? (showDbg$ tail$      bintree 2 $ singleton 1)
   , testCase "inspect tree3"$  "(_ | (1 | 1))"       @=? (showDbg$ tail$tail$ bintree 2 $ singleton 1)
   ],

  testGroup "AList QuickCheck Tests " [
     testProperty "map"     prop_map
   , testProperty "filter"  prop_filter
   , testProperty "tofrom"  prop_tofrom
   , testProperty "tofromB" prop_tofromB
   , testProperty "balance" prop_balance
   ]
  ]

-- main = defaultMain tests
