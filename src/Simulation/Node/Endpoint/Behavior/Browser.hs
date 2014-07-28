{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Simulation.Node.Endpoint.Behavior.Browser
       ( browsePage
       , randomLink
       ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import Network.Http.Client
import Simulation.Node.Endpoint.Behavior
import Simulation.Node.Endpoint.Internal.Relations
import System.Random (randomRIO)
import Text.HTML.TagSoup (parseTags)
import Text.Printf

browsePage :: BehaviorState s => BS.ByteString -> Behavior s [BS.ByteString]
browsePage resource = do
  gateway <- webGateway
  port    <- webPort
  page    <- liftIO $ fetchResource gateway port resource
  liftIO $ printf "Size received: %d\n" (BS.length page)
  let relations' = relations $ parseTags page
  liftIO $ printf " -> %s\n" (show $ images relations')
  void $ liftIO (mapConcurrently (fetchResource gateway port)
                                 (images relations'))
  return $ links relations'

randomLink :: BehaviorState s => [BS.ByteString] -> Behavior s BS.ByteString
randomLink xs = do
  index <- liftIO $ randomRIO (0, length xs - 1)
  return $ xs !! index

fetchResource :: Hostname -> Port -> BS.ByteString -> IO BS.ByteString
fetchResource hostname port resource =
  withConnection (openConnection hostname port) $ \conn -> do
    request <- buildRequest $
      http GET resource
    sendRequest conn request emptyBody
    receiveResponse conn concatHandler

