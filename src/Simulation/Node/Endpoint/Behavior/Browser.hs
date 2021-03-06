{-# LANGUAGE OverloadedStrings, TupleSections, BangPatterns #-}
module Simulation.Node.Endpoint.Behavior.Browser
       ( browsePage
       ) where

import Control.DeepSeq (($!!))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as BS
import Network.Socket (SockAddr)
import Network.Http.Client
import Simulation.Node.Endpoint.AppCounter (AppCounter)
import Simulation.Node.Endpoint.Behavior
import Simulation.Node.Endpoint.Internal.Relations
import qualified System.IO.Streams as Streams
import Text.HTML.TagSoup.Fast (parseTagsT)

browsePage :: (AppCounter c, BehaviorState s) =>
              Text -> Behavior c s [Text]
browsePage resource = do
  addr               <- selfIpAddress
  gateway            <- webGateway
  port               <- webPort
  (relations', size) <- liftIO $ processContent addr gateway port resource
  receivedBytes size
  return $!! links relations'

processContent :: SockAddr -> Hostname -> Port -> Text -> IO (Relations, Int)
processContent addr hostname port resource =
  withConnection (openBoundConnection (Just addr) hostname port) $ \conn -> do
    (page, pageSize) <- 
      sendAndReceive conn receiveResponse contentAndSizeH resource
    let relations' = relations $ parseTagsT page
    fetchSizes <- 
      mapM (sendAndReceive conn receiveResponseRaw sizeH) $ 
        scripts relations' ++ stylesheets relations' ++ images relations'
    return (relations', pageSize + sum fetchSizes)

sendAndReceive :: Connection
               -> (Connection 
                   -> (Response -> 
                       Streams.InputStream BS.ByteString -> IO a) -> IO a)
               -> (Response -> Streams.InputStream BS.ByteString -> IO a)
               -> Text
               -> IO a
sendAndReceive conn receive handler resource = do
  request <- buildRequest $ http GET (encodeUtf8 resource)
  sendRequest conn request emptyBody
  receive conn handler
  
contentAndSizeH :: Response
                -> Streams.InputStream BS.ByteString
                -> IO (BS.ByteString, Int)
contentAndSizeH response stream = do
  content <- concatHandler response stream
  return (content, BS.length content)

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
      
  
