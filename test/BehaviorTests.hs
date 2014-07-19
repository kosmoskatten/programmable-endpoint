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
  )

suite :: Test.Framework.Test
suite = testGroup "Behavior tests"
        [ testCase "Slogan shall be returned back"
                   sloganShallBeReturnedBack
        , testCase "Unit shall be returned back"
                   unitShallBeReturnedBack                   
        , testCase "Initial state shall be returned back"
                   initialStateShallBeReturnedBack
        ]

data TestState = InitialState
  deriving (Eq, Show)

instance BehaviorState TestState where
  fetch = return ("TestSlogan", InitialState)

emptyAction :: Behavior TestState ()
emptyAction = return ()

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

makeApiParam :: BehaviorApiParam
makeApiParam = BehaviorApiParam "127.0.0.1"
      
