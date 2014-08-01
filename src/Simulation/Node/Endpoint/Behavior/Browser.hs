{-# LANGUAGE OverloadedStrings, TupleSections, BangPatterns #-}
module Simulation.Node.Endpoint.Behavior.Browser
       ( browsePage
       , randomLink
       ) where

import qualified Data.ByteString.Char8 as BS
import Network.Http.Client
import Simulation.Node.Endpoint.Behavior
import Simulation.Node.Endpoint.Internal.Relations
import qualified System.IO.Streams as Streams
import System.Random (randomRIO)
import Text.HTML.TagSoup.Fast (parseTags)

browsePage :: BehaviorState s => BS.ByteString -> Behavior s [BS.ByteString]
browsePage resource = do
  gateway           <- webGateway
  port              <- webPort
  (relations', size) <- liftIO $ processContent gateway port resource
  updateBytesReceived size
  return $ links relations'

processContent :: Hostname -> Port -> BS.ByteString -> IO (Relations, Int)
processContent hostname port resource =
  withConnection (openConnection hostname port) $ \conn -> do
    (page, pageSize) <- receiveWithHandler conn contentAndSizeH resource
    let relations' = relations $ parseTags page
    imageSizes <- mapM (receiveWithHandler conn sizeH) $ images relations'
    return $ (relations', pageSize + sum imageSizes)

receiveWithHandler :: Connection
                   -> (Response -> Streams.InputStream BS.ByteString -> IO a)
                   -> BS.ByteString
                   -> IO a
receiveWithHandler conn handler resource = do
  request <- buildRequest $ http GET resource
  sendRequest conn request emptyBody
  receiveResponse conn handler

randomLink :: BehaviorState s => [BS.ByteString] -> Behavior s BS.ByteString
randomLink xs = do
  index <- liftIO $ randomRIO (0, length xs - 1)
  return $ xs !! index

contentAndSizeH :: Response
                -> Streams.InputStream BS.ByteString
                -> IO (BS.ByteString, Int)
contentAndSizeH response stream = do
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
      
  
