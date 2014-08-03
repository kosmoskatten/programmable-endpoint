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
removeEndpoint :: (Ord c, Counter c) => Endpoint c -> Node c -> IO ()
removeEndpoint endpoint node = do
  reset endpoint
  atomically $ modifyTVar' (endpoints node) (delete endpoint)

-- | List all endpoints.
listAll :: Node c -> IO [Endpoint c]
listAll node = readTVarIO (endpoints node)
