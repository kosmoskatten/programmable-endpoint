{-# LANGUAGE OverloadedStrings, TupleSections, BangPatterns #-}
module Simulation.Node.Endpoint.Behavior.Browser
       ( browsePage
       , randomLink
       ) where

import Control.Concurrent.Async (mapConcurrently)
import qualified Data.ByteString.Char8 as BS
import Network.Http.Client
import Simulation.Node.Endpoint.Behavior
import Simulation.Node.Endpoint.Internal.Relations
import qualified System.IO.Streams as Streams
import System.Random (randomRIO)
import Text.HTML.TagSoup.Fast (parseTags)

browsePage :: BehaviorState s => BS.ByteString -> Behavior s [BS.ByteString]
browsePage resource = do
  gateway      <- webGateway
  port         <- webPort
  (page, size) <- liftIO $ getWithHandler gateway port contentAndSize resource
  let relations' = relations $ parseTags page
  sizes <- liftIO (mapConcurrently (getWithHandler gateway port sizeH)
                                   (images relations'))
  updateBytesReceived $ size + sum sizes
  return $ links relations'

randomLink :: BehaviorState s => [BS.ByteString] -> Behavior s BS.ByteString
randomLink xs = do
  index <- liftIO $ randomRIO (0, length xs - 1)
  return $ xs !! index

getWithHandler :: Hostname
               -> Port
               -> (Response -> Streams.InputStream BS.ByteString -> IO a)
               -> BS.ByteString
               -> IO a
getWithHandler hostname port handler resource=
  withConnection (openConnection hostname port) $ \conn -> do
    request <- buildRequest $ http GET resource
    sendRequest conn request emptyBody
    receiveResponse conn handler

contentAndSize :: Response
               -> Streams.InputStream BS.ByteString
               -> IO (BS.ByteString, Int)
contentAndSize response stream = do
  content <- concatHandler response stream
  return $ (content, BS.length content)

sizeH  :: Response -> Streams.InputStream BS.ByteString -> IO Int
sizeH _ stream =
  go 0
  where
    go :: Int -> IO Int
    go !acc = do
      cont <- Streams.read stream
      case cont of
        Just chunk -> go (acc + BS.length chunk)
        Nothing    -> return acc
      
  
