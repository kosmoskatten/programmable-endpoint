module Main
       ( main
       ) where

import Test.Framework (Test, defaultMain)
import qualified NodeTests as NT
import qualified EndpointTests as EPT
import qualified BehaviorTests as BT
import qualified HttpServiceTests as HST

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite = 
  [ NT.suite
  , EPT.suite
  , BT.suite
  , HST.suite
  ]
