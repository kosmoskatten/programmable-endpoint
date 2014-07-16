module EndpointTests
       ( suite
       ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Simulation.Node.Endpoint (create, destroy)

suite :: Test.Framework.Test
suite = testGroup "Endpoint tests"
        [ testCase "Shall be different endpoints for different Ip's"
                   createWithDifferentIpsShallGiveDifferentEndpoints
        ]

createWithDifferentIpsShallGiveDifferentEndpoints :: Assertion
createWithDifferentIpsShallGiveDifferentEndpoints = do
  ep1 <- create "127.0.0.1"
  ep2 <- create "127.0.0.2"
  assertBool "Shall be different" $ ep1 /= ep2
  destroy ep1
  destroy ep2

