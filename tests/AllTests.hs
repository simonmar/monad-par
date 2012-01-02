
-- | Combine individual test modules into a single module.

module Main where 

import Test.Framework (defaultMain, testGroup)
import qualified AListTest 
import qualified ParTests
import qualified AsyncTest

main = defaultMain $ concat $ 
       [ AListTest.tests
       , ParTests.tests
-- Not working right now:
--       , AsyncTests.tests
       ]

-- main = AsyncTests.manual_main
