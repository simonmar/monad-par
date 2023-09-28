
-- | Combine individual test modules into a single module.

module Main where 

import Test.Tasty (defaultMain, testGroup)
import qualified AListTest 
import qualified AsyncTest
import qualified ParTests1
import qualified ParTests2

main = defaultMain $ testGroup "All monad-par tests"
       [ AListTest.tests
       , ParTests1.tests
--       , ParTests2.tests -- [2013.09.08] Temporarily disabling till we get back to debugging Direct.
-- Not working right now:
--       , AsyncTests.tests
       ]

-- main = AsyncTests.manual_main
