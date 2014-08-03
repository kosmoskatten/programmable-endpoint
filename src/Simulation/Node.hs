module Simulation.Node
       ( Node (counter)
       , Simulation.Node.create
       , createEndpoint
       , removeEndpoint
       , Simulation.Node.listAll
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  )
import qualified Data.Set as Set
import Simulation.Node.Counter
import qualified Simulation.Node.Counter as Counter
import Simulation.Node.Endpoint
import qualified Simulation.Node.Endpoint as Endpoint
import Simulation.Node.Endpoint.Behavior
  ( Hostname
  , Port
  )

type EndpointSet c = Set.Set (Endpoint c)

-- | A node instance descriptor.
data Node c =
  Node { webGateway  :: !Hostname
       , webPort     :: !Port
       , endpointSet :: TVar (EndpointSet c)
       , counter     :: TVar c}
  deriving Eq

-- | Create a node instance.
create :: Counter c => Hostname -> Port -> IO (Node c)
create gateway port = Node gateway port <$> newTVarIO Set.empty
                                        <*> newTVarIO Counter.empty

-- | Create an endpoint instance.
createEndpoint :: (Ord c, Counter c) => IpAddress -> Node c -> IO (Endpoint c)
createEndpoint ip node = do
  endpoint <- Endpoint.create ip (webGateway node) (webPort node)
  atomically $ modifyTVar' (endpointSet node)
                           (\s -> Set.insert endpoint s)
  return endpoint

-- | Remove an endpoint from the node.
removeEndpoint :: (Ord c, Counter c) => Endpoint c -> Node c -> IO ()
removeEndpoint endpoint node = do
  reset endpoint
  atomically $ modifyTVar' (endpointSet node)
                           (\s -> Set.delete endpoint s)

-- | List all endpoints.
listAll :: Node c -> IO [Endpoint c]
listAll node = Set.toList <$> readTVarIO (endpointSet node)
