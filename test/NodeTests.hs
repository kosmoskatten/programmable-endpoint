{-# LANGUAGE OverloadedStrings #-}
module NodeTests
       ( suite
       ) where

import Control.Applicative ((<$>))
import Control.Concurrent.STM (TVar, atomically, readTVarIO)
import Control.Concurrent.STM.TMVar
  ( TMVar
  , newEmptyTMVarIO
  , putTMVar
  , takeTMVar
  )
import Control.Monad (void)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Node)
import Simulation.Node
import qualified Simulation.Node as Node
import Simulation.Node.Counter (Counter (..))
import Simulation.Node.Endpoint
import qualified Simulation.Node.Endpoint as Endpoint
import Simulation.Node.Endpoint.Behavior
  ( Behavior
  , BehaviorState (..)
  , Hostname
  , Port
  , updateBytesReceived
  , liftIO
  )
import GHC.Int

suite :: Test.Framework.Test
suite = testGroup "Node tests"
        [ testCase "Shall list correct number of endpoints"
                   shallListCorrectNumberOfEndpoints
        , testCase "All counters shall be updated equally"
                   shallUpdateCountersEqually
        ]

data TestState = TestState

instance BehaviorState TestState where
  fetch = return ("TestSlogan", TestState)

data TestCounter = TestCounter Int64
  deriving (Eq, Ord)

instance Counter TestCounter where
  empty                              = TestCounter 0
  addReceived amount (TestCounter v) = TestCounter $ v + fromIntegral amount
  getReceived (TestCounter v)        = v

-- | Simple behavior that is adding one to the bytes received counter
addingBehavior :: TMVar () -> Behavior TestCounter TestState ()
addingBehavior sync = do
  updateBytesReceived 1
  liftIO $ putTMVarIO sync ()

-- | Test that the node is listing the correct number of endpoints.
shallListCorrectNumberOfEndpoints :: Assertion
shallListCorrectNumberOfEndpoints = do
  node <- (Node.create gateway port) :: IO (Node TestCounter)
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
              1 =<< getByteCount (Endpoint.theCounter b)
  assertEqual "Endpoint counter shall be 1"
              1 =<< getByteCount (Endpoint.counter ep)
  assertEqual "Node counter shall be 1"
              1 =<< getByteCount (Node.counter node)

getByteCount :: TVar TestCounter -> IO Int64
getByteCount tvar = getReceived <$> readTVarIO tvar

putTMVarIO :: TMVar a -> a -> IO ()
putTMVarIO tmvar v = atomically $ putTMVar tmvar v

takeTMVarIO :: TMVar a -> IO a
takeTMVarIO tmvar = atomically $ takeTMVar tmvar

gateway :: Hostname
gateway = "192.168.100.1"

port :: Port
port = 8888

        
