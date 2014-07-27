module Simulation.Node.Endpoint.Behavior.Browser
       ( browsePage
       ) where

import qualified Data.ByteString as BS
import Network.Http.Client
import Simulation.Node.Endpoint.Behavior

browsePage :: BehaviorState s => BS.ByteString -> Behavior s BS.ByteString
browsePage resource = do
  gateway <- webGateway
  port    <- webPort
  page    <- liftIO $ fetchResource gateway port resource
  return page

fetchResource :: Hostname -> Port -> BS.ByteString -> IO BS.ByteString
fetchResource hostname port resource =
  withConnection (openConnection hostname port) $ \conn -> do
    request <- buildRequest $
      http GET resource
    sendRequest conn request emptyBody
    receiveResponse conn concatHandler
