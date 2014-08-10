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
import Simulation.Node.SystemCounter
import qualified Simulation.Node.SystemCounter as SC
import Simulation.Node.Endpoint
  ( reset
  , addBehavior
  , removeBehavior
  , listAll
  )
import qualified Simulation.Node.Endpoint as Endpoint
import Simulation.Node.Endpoint.AppCounter (AppCounter (..))
import Simulation.Node.Endpoint.Behavior
  ( Behavior
  , BehaviorState (..)
  , Hostname
  , Port
  , get
  , liftIO
  , sleepMsec
  )
import Simulation.Node.Endpoint.Behavior.Descriptor (Descriptor (..))

suite :: Test.Framework.Test
suite = testGroup "Endpoint tests"
        [ testCase "Shall be different endpoints"
                   shallBeDifferentEndpoints
        , testCase "Shall be different receipts"
                   shallBeDifferentReceipts
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
        , testCase "Shall remove all behaviors at reset"
                   shallRemoveAllBehaviorsAtReset
        , testCase "Shall list correct number of behaviors"
                   shallListCorrectNumberOfBehaviors
        , testCase "Shall list corrent slogan"
                   shallListCorrectSlogan
        ]

data TestState = TestState {num :: !Int}
  deriving (Eq, Show)

instance BehaviorState TestState where
  fetch = ("TestSlogan",) <$> (TestState <$> randomRIO (minBound, maxBound))

data Counter = Counter
  deriving (Eq, Show)

instance AppCounter Counter where
  create = Counter

emptyAction :: Behavior Counter TestState ()
emptyAction = return ()

countingAction :: TVar Int -> Behavior Counter TestState ()
countingAction tvar =
  forever $ do
    liftIO $ atomically (modifyTVar tvar (+ 1))
    sleepMsec 10

crashingAction :: TMVar Int -> Behavior Counter TestState ()
crashingAction tmvar = do
  num' <- num <$> get
  liftIO $ atomically (putTMVar tmvar num')
  liftIO $ print (1 `div` 0 :: Int)

terminatingAction :: TMVar () -> Behavior Counter TestState ()
terminatingAction tmvar =
  liftIO $ atomically (putTMVar tmvar ())

-- | Test that two endpoints, for two different, IP addresses are
-- unequal.
shallBeDifferentEndpoints :: Assertion
shallBeDifferentEndpoints = do
  c   <- nodeCounter
  ep1 <- (Endpoint.create localhost gateway port c)
         :: IO (Endpoint.Endpoint Counter)
  ep2 <- Endpoint.create "127.0.0.2" gateway port c
  assertBool "Shall be different" $ ep1 /= ep2

-- | Test that two receipts gotten the same endpoint are unequal.
shallBeDifferentReceipts :: Assertion
shallBeDifferentReceipts = do
  c  <- nodeCounter
  ep <- Endpoint.create localhost gateway port c
  r1 <- addBehavior emptyAction ep
  r2 <- addBehavior emptyAction ep
  assertBool "Shall be different" $ r1 /= r2

-- | Test that an added behavior run in its own thread.
shallRunInItsOwnThread :: Assertion
shallRunInItsOwnThread = do
  c  <- nodeCounter
  ep <- Endpoint.create localhost gateway port c
  tvar <- newTVarIO 0
  maybeR <- timeout 100000 $ addBehavior (countingAction tvar) ep
  case maybeR of
    Just _ ->
      assertBool "Counter shall have progressed" =<< isProgressing tvar      
    _      -> assertBool "addBehavior is blocking - not threaded?" False

-- | Test that a behavior thread is stopped when the behavior is
-- removed from its endpoint.
shallStopWhenRemoved :: Assertion
shallStopWhenRemoved = do
  c    <- nodeCounter
  ep   <- Endpoint.create localhost gateway port c
  tvar <- newTVarIO 0
  r    <- addBehavior (countingAction tvar) ep
  void $ removeBehavior ep r
  assertBool "Counter shall not have progressed"
             =<< not <$> isProgressing tvar

-- | Test that a crashing behavior is restarted by its supervisor.
shallRestartWhenCrashed :: Assertion
shallRestartWhenCrashed = do
  c     <- nodeCounter
  ep    <- Endpoint.create localhost gateway port c
  tmvar <- newEmptyTMVarIO
  void $ addBehavior (crashingAction tmvar) ep
  void $ atomically (takeTMVar tmvar) -- First start
  maybeResult <- timeout 100000 $ atomically (takeTMVar tmvar)
  case maybeResult of
    Just _ -> return ()
    _      -> assertBool "Behavior shall have been restarted" False

-- | Test that a normally terminating behavior not is restarted by its
-- supervisor.
shallNotRestartWhenTerminated :: Assertion
shallNotRestartWhenTerminated = do
  c     <- nodeCounter
  ep    <- Endpoint.create localhost gateway port c
  tmvar <- newEmptyTMVarIO
  void $ addBehavior (terminatingAction tmvar) ep
  void $ atomically (takeTMVar tmvar)
  maybeResult <- timeout 100000 $ atomically (takeTMVar tmvar)
  case maybeResult of
    Just () -> assertBool "Behavior shall not have been restarted" False
    _       -> return ()

-- | Test that a restarting behavior always get the same initial state
-- (the example state is randomly generated).
shallGetTheSameInitialStateAtRestart :: Assertion
shallGetTheSameInitialStateAtRestart = do
  c       <- nodeCounter
  ep      <- Endpoint.create localhost gateway port c
  tmvar   <- newEmptyTMVarIO
  void $ addBehavior (crashingAction tmvar) ep
  result  <- atomically (takeTMVar tmvar) -- First start
  result' <- atomically (takeTMVar tmvar) -- Second start
  assertEqual "Shall be equal" result result'

-- | Test that behaviors are terminated when an endpoint is reset.
shallRemoveAllBehaviorsAtReset :: Assertion
shallRemoveAllBehaviorsAtReset = do
  c     <- nodeCounter
  ep    <- Endpoint.create localhost gateway port c
  tvar1 <- newTVarIO 0
  tvar2 <- newTVarIO 0
  void $ addBehavior (countingAction tvar1) ep
  void $ addBehavior (countingAction tvar2) ep
  reset ep
  assertBool "Counter shall not have progressed"
             =<< not <$> isProgressing tvar1
  assertBool "Counter shall not have progressed"
             =<< not <$> isProgressing tvar2

-- | Test that the correct number of behaviors are listed.
shallListCorrectNumberOfBehaviors :: Assertion
shallListCorrectNumberOfBehaviors = do
  c  <- nodeCounter
  ep <- Endpoint.create localhost gateway port c
  assertEqual "Shall be empty"
               0 =<< length <$> listAll ep
  b1 <- addBehavior emptyAction ep
  assertEqual "Shall be 1"
               1 =<< length <$> listAll ep
  b2 <- addBehavior emptyAction ep
  assertEqual "Shall be 2"
               2 =<< length <$> listAll ep
  void $ removeBehavior ep b2
  assertEqual "Shall be 1"
               1 =<< length <$> listAll ep
  void $ removeBehavior ep b1
  assertEqual "Shall be empty"
               0 =<< length <$> listAll ep

-- | Test that the correct slogan is listed for a behavior
shallListCorrectSlogan :: Assertion
shallListCorrectSlogan = do
  c  <- nodeCounter
  ep <- Endpoint.create localhost gateway port c
  void $ addBehavior emptyAction ep
  theSlogan <- slogan . head <$> listAll ep
  assertEqual "Shall be equal"
              "TestSlogan" theSlogan
  
-- | Check if a TVar protected counter is progressing during 1/10th of
-- a second.
isProgressing :: TVar Int -> IO Bool
isProgressing tvar = do
  sample <- readTVarIO tvar
  threadDelay 100000
  sample' <- readTVarIO tvar
  return $ sample' > sample

localhost :: String
localhost = "127.0.0.1"

gateway :: Hostname
gateway = "192.168.100.1"

port :: Port
port = 8888

nodeCounter :: IO (TVar SystemCounter)
nodeCounter = newTVarIO SC.create
