module Simulation.Node.Endpoint.Behavior.Browser where

import qualified Data.ByteString as BS
import Network.Http.Client
import Simulation.Node.Endpoint.Behavior

browseAt :: BehaviorState s => BS.ByteString -> Behavior s BS.ByteString
browseAt whereTo = return BS.empty
