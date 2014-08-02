{-# LANGUAGE OverloadedStrings #-}
module BehaviorTests
       ( suite
       ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Simulation.Node.Counter
import Simulation.Node.Endpoint.Behavior
  ( Behavior
  , BehaviorApiParam (..)
  , BehaviorState (..)
  , Hostname
  , Port
  , runBehaviorTest
  , get
  , put
  , selfIpAddress
  , webGateway
  , webPort
  )

suite :: Test.Framework.Test
suite = testGroup "Behavior tests"
        [ testCase "Slogan shall be returned back"
                   sloganShallBeReturnedBack
        , testCase "Unit shall be returned back"
                   unitShallBeReturnedBack                   
        , testCase "Initial state shall be returned back"
                   initialStateShallBeReturnedBack
        , testCase "Stepped state shall be returned back"
                   steppedStateShallBeReturnedBack
        , testCase "Initial state shall be returned back as value"
                   initialStateShallBeReturnedBackAsValue
        , testCase "Api selfIpAddress shall return api param value"
                   selfIpAddressShallReturnApiParamValue
        , testCase "Api webGateway shall return api param value"
                   webGatewayShallReturnApiParamValue
        , testCase "Api webPort shall return api param value"
                   webPortShallReturnApiParamValue
        ]

data TestState = InitialState | SteppedState
  deriving (Eq, Show)

instance BehaviorState TestState where
  fetch = return ("TestSlogan", InitialState)

data TestCounter = TestCounter

instance DataSet TestCounter where
  empty = TestCounter

instance ByteCounter TestCounter where
  addReceived _ x = x
  getReceived _   = 0

emptyAction :: Behavior TestCounter TestState ()
emptyAction = return ()

stepStateAction :: Behavior TestCounter TestState ()
stepStateAction = do
  put SteppedState

getStateAction :: Behavior TestCounter TestState TestState
getStateAction = get

selfIpAddress' :: Behavior TestCounter TestState String
selfIpAddress' = selfIpAddress

webGateway' :: Behavior TestCounter TestState Hostname
webGateway' = webGateway

webPort' :: Behavior TestCounter TestState Port
webPort' = webPort

-- | Unit test with the empty action to make sure that the slogan is
-- retured back from runBehaviorTest.
sloganShallBeReturnedBack :: Assertion
sloganShallBeReturnedBack = do
  (slogan, _, _) <- runBehaviorTest emptyAction makeApiParam
  assertEqual "Shall be equal"
              "TestSlogan" slogan

-- | Unit test with empty action to make sure that () is returned back
-- from runBehaviorTest.
unitShallBeReturnedBack :: Assertion
unitShallBeReturnedBack = do
  (_, unit, _) <- runBehaviorTest emptyAction makeApiParam
  assertEqual "Shall be equal"
              () unit

-- | Unit test with empty action to make sure that the initial state
-- is returned back from runBehaviorTest.
initialStateShallBeReturnedBack :: Assertion
initialStateShallBeReturnedBack = do
  (_, _, initialState) <- runBehaviorTest emptyAction makeApiParam
  assertEqual "Shall be equal"
              InitialState initialState

-- | Unit test with an action that steps the state. Make sure that the
-- stepped state is returned back from runBehaviorTest.
steppedStateShallBeReturnedBack :: Assertion
steppedStateShallBeReturnedBack = do
  (_, _, steppedState) <- runBehaviorTest stepStateAction makeApiParam
  assertEqual "Shall be equal"
              SteppedState steppedState

-- | Unit test with an action that returns the initial state. Make
-- sure that the state is retured as a return value.
initialStateShallBeReturnedBackAsValue :: Assertion
initialStateShallBeReturnedBackAsValue = do
  (_, initialState, _) <- runBehaviorTest getStateAction makeApiParam
  assertEqual "Shall be equal"
              InitialState initialState

-- | Test that selfIpAddress return the value from api params.
selfIpAddressShallReturnApiParamValue :: Assertion
selfIpAddressShallReturnApiParamValue = do
  (_, ipAddress, _) <- runBehaviorTest selfIpAddress' makeApiParam
  assertEqual "Shall be equal"
              localhost ipAddress

-- | Test that webGateway return the value from api params.
webGatewayShallReturnApiParamValue :: Assertion
webGatewayShallReturnApiParamValue = do
  (_, ipGateway, _) <- runBehaviorTest webGateway' makeApiParam
  assertEqual "Shall be equal"
              gateway ipGateway

-- | Test that webPort return the value from api params.
webPortShallReturnApiParamValue :: Assertion
webPortShallReturnApiParamValue = do
  (_, gatewayPort, _) <- runBehaviorTest webPort' makeApiParam
  assertEqual "Shall be equal"
              port gatewayPort

makeApiParam :: BehaviorApiParam c
makeApiParam = BehaviorApiParam localhost gateway port []
      
localhost :: String
localhost = "127.0.0.1"

gateway :: Hostname
gateway = "192.168.1.100"

port :: Port
port = 8888
