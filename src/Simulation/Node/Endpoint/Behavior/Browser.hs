{-# LANGUAGE OverloadedStrings, TupleSections, BangPatterns #-}
module Simulation.Node.Endpoint.Behavior.Browser
       ( browsePage
       , randomLink
       ) where

import Control.DeepSeq (($!!))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as BS
import Network.Http.Client
import Simulation.Node.Counter (DataSet, ByteCounter)
import Simulation.Node.Endpoint.Behavior
import Simulation.Node.Endpoint.Internal.Relations
import qualified System.IO.Streams as Streams
import System.Random (randomRIO)
import Text.HTML.TagSoup.Fast (parseTagsT)

browsePage :: (DataSet c, ByteCounter c, BehaviorState s) =>
              Text -> Behavior c s [Text]
browsePage resource = do
  gateway           <- webGateway
  port              <- webPort
  (relations', size) <- liftIO $ processContent gateway port resource
  updateBytesReceived size
  return $!! links relations'

randomLink :: (DataSet c, ByteCounter c, BehaviorState s) =>
              [a] -> Behavior c s a
randomLink xs = do
  index <- liftIO $ randomRIO (0, length xs - 1)
  return $ xs !! index

processContent :: Hostname -> Port -> Text -> IO (Relations, Int)
processContent hostname port resource =
  withConnection (openConnection hostname port) $ \conn -> do
    (page, pageSize) <- receiveWithHandler conn contentAndSizeH resource
    let relations' = relations $ parseTagsT page
    imageSizes <- mapM (receiveWithHandler conn sizeH) $ images relations'
    return $ (relations', pageSize + sum imageSizes)

receiveWithHandler :: Connection
                   -> (Response -> Streams.InputStream BS.ByteString -> IO a)
                   -> Text
                   -> IO a
receiveWithHandler conn handler resource = do
  request <- buildRequest $ http GET (encodeUtf8 resource)
  sendRequest conn request emptyBody
  receiveResponse conn handler

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
      
  
