
-- | Combine individual test modules into a single module.

module Main where 

import Test.Framework (defaultMain, testGroup)
import qualified AListTest 
import qualified AsyncTest
import qualified ParTests1
import qualified ParTests2

main = defaultMain $ concat $ 
       [ AListTest.tests
       , ParTests1.tests
--       , ParTests2.tests -- [2013.09.08] Temporarily disabling till we get back to debugging Direct.
-- Not working right now:
--       , AsyncTests.tests
       ]

-- main = AsyncTests.manual_main
