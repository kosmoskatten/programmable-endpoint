module Main
       ( main
       ) where

import Test.Framework (Test, defaultMain)
import qualified EndpointTests as EPT

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite = [ EPT.suite ]
