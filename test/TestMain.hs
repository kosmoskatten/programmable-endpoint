module Main
       ( main
       ) where

import Test.Framework (Test, defaultMain)
import qualified EndpointTests as EPT
import qualified BehaviorTests as BT

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite = 
  [ EPT.suite
  , BT.suite ]
