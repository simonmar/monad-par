
module Main where 

import Test.Framework (defaultMain, testGroup)
import qualified AListTest 
import qualified ParTests

main = defaultMain $ concat $ 
       [ AListTest.tests
       , ParTests.tests
       ]
