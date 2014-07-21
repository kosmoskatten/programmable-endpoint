{-# LANGUAGE OverloadedStrings, TupleSections #-}
module EndpointTests
       ( suite
       ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
  ( TVar
  , TMVar
  , atomically
  , modifyTVar
  , newTVarIO
  , readTVarIO
  , newEmptyTMVarIO
  , putTMVar
  , takeTMVar
  )
import Control.Monad (forever, void)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Data.Text ()
import System.Random (randomRIO)
import System.Timeout (timeout)
import Simulation.Node.Endpoint
  ( create
  , addBehavior
  , removeBehavior
  )
import Simulation.Node.Endpoint.Behavior
  ( Behavior
  , BehaviorState (..)
  , get
  , liftIO
  , sleepMsec
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
        , testCase "Shall stop when removed"
                   shallStopWhenRemoved
        , testCase "Shall restart when crashed"
                   shallRestartWhenCrashed
        , testCase "Shall not restart when terminated"
                   shallNotRestartWhenTerminated
        , testCase "Shall always get the same initialState at restart"
                   shallGetTheSameInitialStateAtRestart
        ]

data TestState = TestState {num :: !Int}
  deriving (Eq, Show)

instance BehaviorState TestState where
  fetch = ("TestSlogan",) <$> (TestState <$> randomRIO (minBound, maxBound))

emptyAction :: Behavior TestState ()
emptyAction = return ()

countingAction :: TVar Int -> Behavior TestState ()
countingAction tvar =
  forever $ do
    liftIO $ atomically (modifyTVar tvar (+ 1))
    sleepMsec 10

crashingAction :: TMVar Int -> Behavior TestState ()
crashingAction tmvar = do
  num' <- num <$> get
  liftIO $ atomically (putTMVar tmvar num')
  liftIO $ print (1 `div` 0 :: Int)

terminatingAction :: TMVar () -> Behavior TestState ()
terminatingAction tmvar =
  liftIO $ atomically (putTMVar tmvar ())

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

-- | Test that a behavior thread is stopped when the behavior is
-- removed from its endpoint.
shallStopWhenRemoved :: Assertion
shallStopWhenRemoved = do
  ep   <- create "127.0.0.1"
  tvar <- newTVarIO 0
  r    <- addBehavior (countingAction tvar) ep
  void $ removeBehavior r ep
  assertBool "Counter shall not have progressed"
             =<< not <$> isProgressing tvar

-- | Test that a crashing behavior is restarted by its supervisor.
shallRestartWhenCrashed :: Assertion
shallRestartWhenCrashed = do
  ep    <- create "127.0.0.1"
  tmvar <- newEmptyTMVarIO
  r     <- addBehavior (crashingAction tmvar) ep
  void $ atomically (takeTMVar tmvar) -- First start
  maybeResult <- timeout 100000 $ atomically (takeTMVar tmvar)
  case maybeResult of
    Just _ -> return ()
    _      -> assertBool "Behavior shall have been restarted" False

-- | Test that a normally terminating behavior not is restarted by its
-- supervisor.
shallNotRestartWhenTerminated :: Assertion
shallNotRestartWhenTerminated = do
  ep    <- create "127.0.0.1"
  tmvar <- newEmptyTMVarIO
  r     <- addBehavior (terminatingAction tmvar) ep
  void $ atomically (takeTMVar tmvar)
  maybeResult <- timeout 100000 $ atomically (takeTMVar tmvar)
  case maybeResult of
    Just () -> assertBool "Behavior shall not have been restarted" False
    _       -> return ()

-- | Test that a restarting behavior always get the same initial state
-- (the example state is randomly generated).
shallGetTheSameInitialStateAtRestart :: Assertion
shallGetTheSameInitialStateAtRestart = do
  ep      <- create "127.0.0.1"
  tmvar   <- newEmptyTMVarIO
  r       <- addBehavior (crashingAction tmvar) ep
  result  <- atomically (takeTMVar tmvar) -- First start
  result' <- atomically (takeTMVar tmvar) -- Second start
  assertEqual "Shall be equal" result result'

-- | Check if a TVar protected counter is progressing during 1/10th of
-- a second.
isProgressing :: TVar Int -> IO Bool
isProgressing tvar = do
  sample <- readTVarIO tvar
  threadDelay 100000
  sample' <- readTVarIO tvar
  return $ sample' > sample
