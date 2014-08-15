{-# LANGUAGE OverloadedStrings #-}
module NodeTests
       ( suite
       ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
  ( TVar
  , TMVar
  , atomically
  , readTVarIO
  , newEmptyTMVarIO
  , putTMVar
  , takeTMVar    
  )
import Control.Monad (void, replicateM)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Node)
import Simulation.Node
import qualified Simulation.Node as Node
import Simulation.Node.SystemCounter (SystemCounter (..))
import Simulation.Node.Endpoint
import qualified Simulation.Node.Endpoint as Endpoint
import Simulation.Node.Endpoint.AppCounter (AppCounter (..))
import Simulation.Node.Endpoint.Behavior
  ( Behavior
  , BehaviorState (..)
  , receivedBytes
  , liftIO
  )
import qualified Simulation.Node.Endpoint.Behavior.Descriptor as Desc
import GHC.Int

suite :: Test.Framework.Test
suite = testGroup "Node tests"
        [ testCase "Shall list correct number of endpoints"
                   shallListCorrectNumberOfEndpoints
        , testCase "All counters shall be updated equally"
                   shallUpdateBytesReceivedEqually
        , testCase "Counters shall be updated hierarchally"
                   shallUpdateBytesReceivedHierarchally
        , testCase "All counters shall be updated equally"
                   shallUpdateActiveBehaviorsEqually
        , testCase "All counters shall be updated equally"
                   shallUpdateBehaviorRestartsEqually
        ]

data TestState = TestState

instance BehaviorState TestState where
  fetch = return ("TestSlogan", TestState)

data Counter = Counter
  deriving (Eq, Show)

instance AppCounter Counter where
  create = Counter

-- | Simple behavior that is adding one to the bytes received counter
addingBehavior :: TMVar () -> Behavior Counter TestState ()
addingBehavior sync = do
  receivedBytes 1
  liftIO $ putTMVarIO sync ()

-- | Simple behavior that is synching with the test case.
synchingBehavior :: TMVar () -> TMVar () -> Behavior Counter TestState ()
synchingBehavior sync1 sync2 = do
  liftIO $ putTMVarIO sync1 ()
  liftIO $ takeTMVarIO sync2

-- | Simple behavior that is crashing in synch with the test case :-)
synchingCrashBehavior :: TMVar () -> TMVar () -> Behavior Counter TestState ()
synchingCrashBehavior sync1 sync2 = do
  liftIO $ putTMVarIO sync1 ()
  liftIO $ takeTMVarIO sync2
  liftIO $ print (1 `div` 0 :: Int)  

-- | Test that the node is listing the correct number of endpoints.
shallListCorrectNumberOfEndpoints :: Assertion
shallListCorrectNumberOfEndpoints = do
  node <- (Node.create gateway port) :: IO (Node Counter)
  assertEqual "Shall be empty"
              0 =<< length <$> endpoints' node
  ep1 <- createEndpoint "127.0.0.1" node
  assertEqual "Shall be 1"
              1 =<< length <$> endpoints' node
  ep2 <- createEndpoint "127.0.0.2" node
  assertEqual "Shall be 2"
              2 =<< length <$> endpoints' node
  removeEndpoint ep1 node
  assertEqual "Shall be 1"
              1 =<< length <$> endpoints' node
  removeEndpoint ep2 node
  assertEqual "Shall be empty"
              0 =<< length <$> endpoints' node
  where
    endpoints' = readTVarIO . endpoints

-- | Test that counters on node, endpoint and behavior level are
-- updated equally.
shallUpdateBytesReceivedEqually :: Assertion
shallUpdateBytesReceivedEqually = do
  sync <- newEmptyTMVarIO
  node <- Node.create gateway port
  ep   <- createEndpoint "127.0.0.1" node
  b    <- addBehavior (addingBehavior sync) ep
  void $ takeTMVarIO sync
  assertEqual "Behavior counter shall be 1"
              1 =<< getByteCount (Desc.counter b)
  assertEqual "Endpoint counter shall be 1"
              1 =<< getByteCount (Endpoint.counter ep)
  assertEqual "Node counter shall be 1"
              1 =<< getByteCount (Node.counter node)

-- | Test that counters on node, endpoints and behaviors are updated
-- hierarchally. I.e. that counter values are accumulated on node and
-- endpoint levels.
shallUpdateBytesReceivedHierarchally :: Assertion
shallUpdateBytesReceivedHierarchally = do
  syncs@[s1, s2, s3, s4] <- replicateM 4 newEmptyTMVarIO
  node <- Node.create gateway port
  ep1  <- createEndpoint "127.0.0.1" node
  ep2  <- createEndpoint "127.0.0.2" node
  b1   <- addBehavior (addingBehavior s1) ep1
  b2   <- addBehavior (addingBehavior s2) ep1
  b3   <- addBehavior (addingBehavior s3) ep2
  b4   <- addBehavior (addingBehavior s4) ep2
  mapM_ takeTMVarIO syncs
  assertEqual "Behavior counter shall be 1"
              1 =<< getByteCount (Desc.counter b1)
  assertEqual "Behavior counter shall be 1"
              1 =<< getByteCount (Desc.counter b2)
  assertEqual "Behavior counter shall be 1"
              1 =<< getByteCount (Desc.counter b3)
  assertEqual "Behavior counter shall be 1"
              1 =<< getByteCount (Desc.counter b4)
  assertEqual "Endpoint counter shall be 2"
              2 =<< getByteCount (Endpoint.counter ep1)
  assertEqual "Endpoint counter shall be 2"
              2 =<< getByteCount (Endpoint.counter ep2)
  assertEqual "Node counter shall be 4"
              4 =<< getByteCount (Node.counter node)              

-- | Test that counters on node, endpoint and behavior level are
-- updated equally.
shallUpdateActiveBehaviorsEqually :: Assertion
shallUpdateActiveBehaviorsEqually = do
  [sync1, sync2] <- replicateM 2 newEmptyTMVarIO
  node <- Node.create gateway port
  ep   <- createEndpoint "127.0.0.1" node
  b    <- addBehavior (synchingBehavior sync1 sync2) ep
  takeTMVarIO sync1 -- Behavior is up.
  assertEqual "Behavior counter shall be 1"
              1 =<< getActiveBehaviors (Desc.counter b)
  assertEqual "Endpoint counter shall be 1"
              1 =<< getActiveBehaviors (Endpoint.counter ep)
  assertEqual "Node counter shall be 1"
              1 =<< getActiveBehaviors (Node.counter node)  
  putTMVarIO sync2 () -- Terminate the behavior and give it some time.
  threadDelay $ 1000 * 100
  assertEqual "Behavior counter shall be 0"
              0 =<< getActiveBehaviors (Desc.counter b)
  assertEqual "Endpoint counter shall be 0"
              0 =<< getActiveBehaviors (Endpoint.counter ep)
  assertEqual "Node counter shall be 0"
              0 =<< getActiveBehaviors (Node.counter node)

-- | Test that counters on node, endpoint and behavior level are
-- updated equally.
shallUpdateBehaviorRestartsEqually :: Assertion
shallUpdateBehaviorRestartsEqually = do
  [sync1, sync2] <- replicateM 2 newEmptyTMVarIO
  node <- Node.create gateway port
  ep   <- createEndpoint "127.0.0.1" node
  b    <- addBehavior (synchingCrashBehavior sync1 sync2) ep
  takeTMVarIO sync1 -- Behavior is up.
  assertEqual "Behavior counter shall be 0"
              0 =<< getBehaviorRestarts (Desc.counter b)
  assertEqual "Endpoint counter shall be 0"
              0 =<< getBehaviorRestarts (Endpoint.counter ep)
  assertEqual "Node counter shall be 0"
              0 =<< getBehaviorRestarts (Node.counter node)
  putTMVarIO sync2 () -- Make it crash ...
  takeTMVarIO sync1 -- And wait for it to come up again.
  assertEqual "Behavior counter shall be 1"
              1 =<< getBehaviorRestarts (Desc.counter b)
  assertEqual "Endpoint counter shall be 1"
              1 =<< getBehaviorRestarts (Endpoint.counter ep)
  assertEqual "Node counter shall be 1"
              1 =<< getBehaviorRestarts (Node.counter node)

getByteCount :: TVar SystemCounter -> IO Int64
getByteCount tvar = bytesReceived <$> readTVarIO tvar

getActiveBehaviors :: TVar SystemCounter -> IO Int64
getActiveBehaviors tvar = activeBehaviors <$> readTVarIO tvar

getBehaviorRestarts :: TVar SystemCounter -> IO Int64
getBehaviorRestarts tvar = behaviorRestarts <$> readTVarIO tvar

putTMVarIO :: TMVar a -> a -> IO ()
putTMVarIO tmvar v = atomically $ putTMVar tmvar v

takeTMVarIO :: TMVar a -> IO a
takeTMVarIO tmvar = atomically $ takeTMVar tmvar

gateway :: Hostname
gateway = "192.168.100.1"

port :: Port
port = 8888
