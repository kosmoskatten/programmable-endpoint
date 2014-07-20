{-# LANGUAGE OverloadedStrings #-}
module EndpointTests
       ( suite
       ) where

import Control.Monad (void)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Data.Text ()
import Simulation.Node.Endpoint
  ( create
  , destroy
  , addBehavior
  , removeBehavior
  )
import Simulation.Node.Endpoint.Behavior
  ( Behavior
  , BehaviorState (..)
  )

suite :: Test.Framework.Test
suite = testGroup "Endpoint tests"
        [ testCase "Shall be different endpoints"
                   shallBeDifferentEndpoints
        , testCase "Shall be different receipts"
                   shallBeDifferentReceipts
        , testCase "Shall remove a behavior once"
                   shallRemoveABehaviorOnce
        ]

data TestState = TestState
  deriving (Eq, Show)

instance BehaviorState TestState where
  fetch = return ("TestSlogan", TestState)

emptyAction :: Behavior TestState ()
emptyAction = return ()

-- | Test that two endpoints, for two different, IP addresses are
-- unequal.
shallBeDifferentEndpoints :: Assertion
shallBeDifferentEndpoints = do
  ep1 <- create "127.0.0.1"
  ep2 <- create "127.0.0.2"
  assertBool "Shall be different" $ ep1 /= ep2
  destroy ep1
  destroy ep2

-- | Test that two receipts gotten the same endpoint are unequal.
shallBeDifferentReceipts :: Assertion
shallBeDifferentReceipts = do
  ep <- create "127.0.0.1"
  r1 <- addBehavior emptyAction ep
  r2 <- addBehavior emptyAction ep
  assertBool "Shall be different" $ r1 /= r2
  void $ removeBehavior r1 ep
  void $ removeBehavior r2 ep
  destroy ep

-- | Test that a receipt only can be removed once. The second time an
-- error message shall be returned.
shallRemoveABehaviorOnce :: Assertion
shallRemoveABehaviorOnce = do
  ep <- create "127.0.0.1"
  r  <- addBehavior emptyAction ep
  resultSuccess <- removeBehavior r ep
  resultFailure <- removeBehavior r ep
  assertEqual "Shall be Right ()"
              (Right ()) resultSuccess
  assertBool "Shall be different" $ resultSuccess /= resultFailure
  destroy ep
  

