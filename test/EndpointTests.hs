{-# LANGUAGE OverloadedStrings #-}
module EndpointTests
       ( suite
       ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar
  , newTVarIO
  , readTVarIO
  )
import Control.Monad (forever)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Data.Text ()
import System.Timeout (timeout)
import Simulation.Node.Endpoint
  ( create
  , addBehavior
  , removeBehavior
  )
import Simulation.Node.Endpoint.Behavior
  ( Behavior
  , BehaviorState (..)
  , liftIO
  )

suite :: Test.Framework.Test
suite = testGroup "Endpoint tests"
        [ testCase "Shall be different endpoints"
                   shallBeDifferentEndpoints
        , testCase "Shall be different receipts"
                   shallBeDifferentReceipts
        , testCase "Shall remove a behavior once"
                   shallRemoveABehaviorOnce
        , testCase "Shall run a behavior in its own thread"
                   shallRunInItsOwnThread
        ]

data TestState = TestState
  deriving (Eq, Show)

instance BehaviorState TestState where
  fetch = return ("TestSlogan", TestState)

emptyAction :: Behavior TestState ()
emptyAction = return ()

countingAction :: TVar Int -> Behavior TestState ()
countingAction tvar =
  forever $ do
    liftIO $ atomically (modifyTVar tvar (+ 1))
    liftIO $ threadDelay 10000 -- 1/100th

-- | Test that two endpoints, for two different, IP addresses are
-- unequal.
shallBeDifferentEndpoints :: Assertion
shallBeDifferentEndpoints = do
  ep1 <- create "127.0.0.1"
  ep2 <- create "127.0.0.2"
  assertBool "Shall be different" $ ep1 /= ep2

-- | Test that two receipts gotten the same endpoint are unequal.
shallBeDifferentReceipts :: Assertion
shallBeDifferentReceipts = do
  ep <- create "127.0.0.1"
  r1 <- addBehavior emptyAction ep
  r2 <- addBehavior emptyAction ep
  assertBool "Shall be different" $ r1 /= r2

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

-- | Test that an added behavior run in its own thread.
shallRunInItsOwnThread :: Assertion
shallRunInItsOwnThread = do
  ep <- create "127.0.0.1"
  tvar <- newTVarIO 0
  maybeR <- timeout 100000 $ addBehavior (countingAction tvar) ep
  case maybeR of
    Just r ->
      assertBool "Counter shall have progressed" =<< isProgressing tvar      
    _      -> assertBool "addBehavior is blocking - not threaded?" False

-- | Check if a TVar protected counter is progressing during 1/10th of
-- a second.
isProgressing :: TVar Int -> IO Bool
isProgressing tvar = do
  sample <- readTVarIO tvar
  threadDelay 100000
  sample' <- readTVarIO tvar
  return $ sample' > sample
