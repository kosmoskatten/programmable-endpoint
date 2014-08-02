{-# LANGUAGE OverloadedStrings #-}
module NodeTests
       ( suite
       ) where

import Control.Applicative ((<$>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Node)
import Simulation.Node
import Simulation.Node.Counter (DataSet (..), ByteCounter (..))
import Simulation.Node.Endpoint.Behavior
  ( Hostname
  , Port
  )

suite :: Test.Framework.Test
suite = testGroup "Node tests"
        [ testCase "Shall list correct number of endpoints"
                   shallListCorrectNumberOfEndpoints
        ]

data TestCounter = TestCounter
  deriving (Eq, Ord)

instance DataSet TestCounter where
  empty = TestCounter

instance ByteCounter TestCounter where
  addReceived _ x = x
  getReceived _   = 0

shallListCorrectNumberOfEndpoints :: Assertion
shallListCorrectNumberOfEndpoints = do
  node <- (create gateway port) :: IO (Node TestCounter)
  assertEqual "Shall be empty"
              0 =<< length <$> listAll node
  ep1 <- createEndpoint "127.0.0.1" node
  assertEqual "Shall be 1"
              1 =<< length <$> listAll node
  ep2 <- createEndpoint "127.0.0.2" node
  assertEqual "Shall be 2"
              2 =<< length <$> listAll node
  removeEndpoint ep1 node
  assertEqual "Shall be 1"
              1 =<< length <$> listAll node
  removeEndpoint ep2 node
  assertEqual "Shall be empty"
              0 =<< length <$> listAll node

gateway :: Hostname
gateway = "192.168.100.1"

port :: Port
port = 8888

        
