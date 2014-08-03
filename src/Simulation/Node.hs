module Simulation.Node
       ( Node (counter)
       , Simulation.Node.create
       , createEndpoint
       , removeEndpoint
       , Simulation.Node.listAll
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM
  ( STM
  , TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  )
import Control.Monad (when)
import Data.List (delete)
import Simulation.Node.Counter
import qualified Simulation.Node.Counter as Counter
import Simulation.Node.Endpoint
import qualified Simulation.Node.Endpoint as Endpoint
import Simulation.Node.Endpoint.Behavior
  ( Hostname
  , Port
  )

-- | A node instance descriptor.
data Node c =
  Node { webGateway  :: !Hostname
       , webPort     :: !Port
       , endpoints   :: TVar [Endpoint c]
       , counter     :: TVar c }
  deriving Eq

-- | Create a node instance.
create :: Counter c => Hostname -> Port -> IO (Node c)
create gateway port = Node gateway port <$> newTVarIO []
                                        <*> newTVarIO Counter.empty

-- | Create an endpoint instance.
createEndpoint :: (Ord c, Counter c) => IpAddress -> Node c -> IO (Endpoint c)
createEndpoint ip node = do
  endpoint <- Endpoint.create ip (webGateway node) (webPort node)
  atomically $ modifyTVar' (endpoints node) (endpoint:)
  return endpoint

-- | Remove an endpoint from the node.
removeEndpoint :: (Eq c, Counter c) => Endpoint c -> Node c -> IO ()
removeEndpoint endpoint node = do
  isDeleted <- atomically $ maybeDelete endpoint (endpoints node)
  when isDeleted $ reset endpoint

maybeDelete :: (Eq c, Counter c) => Endpoint c -> TVar [Endpoint c] -> STM Bool
maybeDelete endpoint endpoints' = do
  xs <- readTVar endpoints'
  if endpoint `elem` xs then do
    writeTVar endpoints' (endpoint `delete` xs)
    return True
    else return False

-- | List all endpoints.
listAll :: Node c -> IO [Endpoint c]
listAll node = readTVarIO (endpoints node)
