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
        ]

data TestState = TestState

instance BehaviorState TestState where
  fetch = return ("TestSlogan", TestState)

testAction :: Behavior TestState ()
testAction = return ()

sloganShallBeReturnedBack :: Assertion
sloganShallBeReturnedBack = do
  (slogan, _, _) <- runBehaviorTest testAction makeApiParam
  assertEqual "Shall be equal"
              "TestSlogan" slogan

makeApiParam :: BehaviorApiParam
makeApiParam = BehaviorApiParam "127.0.0.1"
      
