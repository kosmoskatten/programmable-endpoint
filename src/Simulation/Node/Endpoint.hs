module Simulation.Node.Endpoint
       ( Endpoint
       , IpAddress
       , create
       , destroy
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.Async (Async, async, waitCatch)
import Control.Concurrent.STM
  ( TChan
  , atomically
  , newTChanIO
  , readTChan
  , writeTChan
  )

type IpAddress = String

-- | An endpoint instance descriptor.
data Endpoint =
  Endpoint { ipAddress   :: !IpAddress
           , commandChan :: TChan Command
           , replyChan   :: TChan Reply
           , task        :: Async () }
  deriving Eq

-- | Commands sent from the API to the endpoint thread.
data Command =
    CreateReq !Endpoint
  | DestroyReq

-- | Replies from the endpoint thread back to the API.
data Reply =
  CreateConf

-- | Create an endpoint instance.
create :: IpAddress -> IO Endpoint
create theIpAddress = do
  cchan <- newTChanIO
  ep <- Endpoint theIpAddress cchan <$> newTChanIO <*> async (endpoint cchan)
  command ep $ CreateReq ep
  CreateConf <- receive $ replyChan ep
  return ep

-- | Destroy an endpoint instance.
destroy :: Endpoint -> IO ()
destroy ep = do
  command ep DestroyReq
  _ <- waitCatch (task ep)
  return ()

-- | The entry for the endpoint thread.
endpoint :: TChan Command -> IO ()
endpoint cchan = do
  CreateReq ep <- receive cchan
  reply ep CreateConf
  eventLoop ep

-- | Event loop for the endpoint thread
eventLoop :: Endpoint -> IO ()
eventLoop ep = do
  request <- receive (commandChan ep)
  return ()

command :: Endpoint -> Command -> IO ()
command ep = writeTChanIO (commandChan ep)

reply :: Endpoint -> Reply -> IO ()
reply ep = writeTChanIO (replyChan ep)

receive :: TChan a -> IO a
receive = readTChanIO

readTChanIO :: TChan a -> IO a
readTChanIO chan = atomically $ readTChan chan
  
writeTChanIO :: TChan a -> a -> IO ()
writeTChanIO chan value = atomically $ writeTChan chan value
  
  
