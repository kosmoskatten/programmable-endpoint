{-# LANGUAGE OverloadedStrings #-}
module NodeTests
       ( suite
       ) where

import Control.Applicative ((<$>))
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
import Simulation.Node.SystemCounter (SystemCounter (bytesReceived))
import Simulation.Node.Endpoint
import qualified Simulation.Node.Endpoint as Endpoint
import Simulation.Node.Endpoint.AppCounter (AppCounter (..))
import Simulation.Node.Endpoint.Behavior
  ( Behavior
  , BehaviorState (..)
  , receivedBytes
  , liftIO
  )
import GHC.Int

suite :: Test.Framework.Test
suite = testGroup "Node tests"
        [ testCase "Shall list correct number of endpoints"
                   shallListCorrectNumberOfEndpoints
        , testCase "All counters shall be updated equally"
                   shallUpdateCountersEqually
        , testCase "Counter shall be updated hierarchally"
                   shallUpdateCountersHierarchally
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

-- | Test that the node is listing the correct number of endpoints.
shallListCorrectNumberOfEndpoints :: Assertion
shallListCorrectNumberOfEndpoints = do
  node <- (Node.create gateway port) :: IO (Node Counter)
  assertEqual "Shall be empty"
              0 =<< length <$> Node.listAll node
  ep1 <- createEndpoint "127.0.0.1" node
  assertEqual "Shall be 1"
              1 =<< length <$> Node.listAll node
  ep2 <- createEndpoint "127.0.0.2" node
  assertEqual "Shall be 2"
              2 =<< length <$> Node.listAll node
  removeEndpoint ep1 node
  assertEqual "Shall be 1"
              1 =<< length <$> Node.listAll node
  removeEndpoint ep2 node
  assertEqual "Shall be empty"
              0 =<< length <$> Node.listAll node

-- | Test that counters on node, endpoint and behavior level is
-- updated equally.
shallUpdateCountersEqually :: Assertion
shallUpdateCountersEqually = do
  sync <- newEmptyTMVarIO
  node <- Node.create gateway port
  ep   <- createEndpoint "127.0.0.1" node
  b    <- addBehavior (addingBehavior sync) ep
  void $ takeTMVarIO sync
  assertEqual "Behavior counter shall be 1"
              1 =<< getByteCount (Endpoint.theSystemCounter b)
  assertEqual "Endpoint counter shall be 1"
              1 =<< getByteCount (Endpoint.epCounter ep)
  assertEqual "Node counter shall be 1"
              1 =<< getByteCount (Node.counter node)

-- | Test that counters on node, endpoints and behaviors are updated
-- hierarchally. I.e. that counter values are accumulated on node and
-- endpoint levels.
shallUpdateCountersHierarchally :: Assertion
shallUpdateCountersHierarchally = do
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
              1 =<< getByteCount (Endpoint.theSystemCounter b1)
  assertEqual "Behavior counter shall be 1"
              1 =<< getByteCount (Endpoint.theSystemCounter b2)
  assertEqual "Behavior counter shall be 1"
              1 =<< getByteCount (Endpoint.theSystemCounter b3)
  assertEqual "Behavior counter shall be 1"
              1 =<< getByteCount (Endpoint.theSystemCounter b4)
  assertEqual "Endpoint counter shall be 2"
              2 =<< getByteCount (Endpoint.epCounter ep1)
  assertEqual "Endpoint counter shall be 2"
              2 =<< getByteCount (Endpoint.epCounter ep2)
  assertEqual "Node counter shall be 4"
              4 =<< getByteCount (Node.counter node)              
  
getByteCount :: TVar SystemCounter -> IO Int64
getByteCount tvar = bytesReceived <$> readTVarIO tvar

putTMVarIO :: TMVar a -> a -> IO ()
putTMVarIO tmvar v = atomically $ putTMVar tmvar v

takeTMVarIO :: TMVar a -> IO a
takeTMVarIO tmvar = atomically $ takeTMVar tmvar

gateway :: Hostname
gateway = "192.168.100.1"

port :: Port
port = 8888

        
