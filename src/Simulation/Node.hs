module Simulation.Node
       ( Node (endpoints, counter)
       , Hostname
       , Port
       , Simulation.Node.create
       , createEndpoint
       , removeEndpoint
       , activateHttpServices
       , as
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM
  ( STM
  , TVar
  , TMVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , newEmptyTMVarIO
  , readTVar
  , writeTVar
  , putTMVar
  )
import Control.Monad (when)
import Data.List (delete)
import Simulation.Node.SystemCounter
import qualified Simulation.Node.SystemCounter as SC
import Simulation.Node.Endpoint hiding (counter)
import Simulation.Node.Endpoint.AppCounter (AppCounter)
import qualified Simulation.Node.Endpoint as Endpoint
import Simulation.Node.Endpoint.Behavior (Hostname, Port)
import Simulation.Node.Service.Http (Service, as, activate)

-- | A node instance descriptor.
data Node c =
  Node { webGateway   :: !Hostname
       , webPort      :: !Port
       , endpoints    :: TVar [Endpoint c]
       , counter      :: TVar SystemCounter
       , httpServices :: TMVar (Async ())}
  deriving Eq

-- | Create a node instance.
create :: AppCounter c => Hostname -> Port -> IO (Node c)
create gateway port = Node gateway port <$> newTVarIO []
                                        <*> newTVarIO SC.create
                                        <*> newEmptyTMVarIO

-- | Create an endpoint instance.
createEndpoint :: AppCounter c => IpAddress -> Node c -> IO (Endpoint c)
createEndpoint ip node = do
  endpoint <- Endpoint.create ip (webGateway node) (webPort node) (counter node)
  atomically $ modifyTVar' (endpoints node) (endpoint:)
  return endpoint

-- | Remove an endpoint from the node.
removeEndpoint :: (Eq c, AppCounter c) => Endpoint c -> Node c -> IO ()
removeEndpoint endpoint node = do
  isDeleted <- atomically $ maybeDelete endpoint (endpoints node)
  when isDeleted $ reset endpoint

maybeDelete :: (Eq c, AppCounter c) =>
               Endpoint c -> TVar [Endpoint c] -> STM Bool
maybeDelete endpoint endpoints' = do
  xs <- readTVar endpoints'
  if endpoint `elem` xs then do
    writeTVar endpoints' (endpoint `delete` xs)
    return True
    else return False

-- | Activate http services in a separate thread.
activateHttpServices :: Node c -> Int -> [Service ()] -> IO ()
activateHttpServices node port services = do
  thread <- async $ activate port services
  atomically $ putTMVar (httpServices node) thread
