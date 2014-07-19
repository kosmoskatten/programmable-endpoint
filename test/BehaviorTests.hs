{-# LANGUAGE OverloadedStrings #-}
module BehaviorTests
       ( suite
       ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Simulation.Node.Endpoint.Behavior
  ( Behavior
  , BehaviorApiParam (..)
  , BehaviorState (..)
  , runBehaviorTest
  , get
  , put
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
        ]

data TestState = InitialState | SteppedState
  deriving (Eq, Show)

instance BehaviorState TestState where
  fetch = return ("TestSlogan", InitialState)

emptyAction :: Behavior TestState ()
emptyAction = return ()

stepStateAction :: Behavior TestState ()
stepStateAction = do
  put SteppedState

getStateAction :: Behavior TestState TestState
getStateAction = get

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

makeApiParam :: BehaviorApiParam
makeApiParam = BehaviorApiParam "127.0.0.1"
      
