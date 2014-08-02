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

instance DataSet TestCounter where
  empty = TestCounter

instance ByteCounter TestCounter where
  addReceived _ x = x
  getReceived _   = 0

shallListCorrectNumberOfEndpoints :: Assertion
shallListCorrectNumberOfEndpoints = do
  n <- (create gateway port) :: IO (Node TestCounter)
  assertEqual "Shall be empty"
              0 =<< length <$> listAll n

gateway :: Hostname
gateway = "192.168.100.1"

port :: Port
port = 8888

        
