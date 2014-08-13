{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simulation.Node.Endpoint.Behavior
       ( Behavior
       , BehaviorApiParam (..)
       , BehaviorState (..)
       , Hostname
       , Port
       , runBehavior
       , runBehaviorTest
       , get
       , put
       , liftIO
       , selfIpAddress
       , webGateway
       , webPort
       , systemCounters
       , appCounter         
       , sleepSec
       , sleepMsec
       , sleepUsec
       , receivedBytes
       , interval
       , oneOfIO
       , oneOf  
       ) where

import Control.Applicative (Applicative, (<$>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar)
import Control.Monad (void)
import Control.Monad.Reader (ReaderT, MonadIO, runReaderT, liftIO)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get, put)
import Data.Text (Text)
import Network.Socket (SockAddr)
import Network.Http.Client (Hostname, Port)
import Simulation.Node.SystemCounter (SystemCounter, bulk, addBytesReceived)
import Simulation.Node.Endpoint.AppCounter (AppCounter)
import System.Random (randomRIO)

-- | The Behavior monad; c is the counter type, s is the user supplied
-- behavior state and a is the reply type of the action.
newtype Behavior c s a =
  Behavior { extractBehavior :: ReaderT (BehaviorApiParam c) (StateT s IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader (BehaviorApiParam c), MonadState s )

-- | Record with api parameters for the execution of the Behavior monad.
data BehaviorApiParam c =
  BehaviorApiParam { selfIpAddress_  :: !SockAddr
                   , webGateway_     :: !Hostname
                   , webPort_        :: !Port
                   , systemCounters_ :: ![TVar SystemCounter]
                   , appCounter_     :: TVar c }

-- | Typeclass for the user supplied behavior state.
class BehaviorState a where
  -- Fetch the initial state and a slogan text for the behavior.
  fetch :: IO (Text, a)

-- | Run a behavior.
runBehavior :: (AppCounter c, BehaviorState s)  =>
               Behavior c s ()    ->
               BehaviorApiParam c ->
               s                  -> IO ()
runBehavior action param initialState =
  void $ runStateT (runReaderT (extractBehavior action) param) initialState

-- | Run a behavior in a way suitable for testing of behaviors.
runBehaviorTest :: (AppCounter c, BehaviorState s)  =>
                   Behavior c s a     ->
                   BehaviorApiParam c -> IO (Text, a, s) 
runBehaviorTest action param = do
  (slogan, initialState) <- fetch
  (result, state)        <-
    runStateT (runReaderT (extractBehavior action) param) initialState
  return (slogan, result, state)

-- | Fetch own's ip address.
selfIpAddress :: (AppCounter c, BehaviorState s) => Behavior c s SockAddr
selfIpAddress = selfIpAddress_ <$> ask

-- | Fetch the gateway ip address to use.
webGateway :: (AppCounter c, BehaviorState s) => Behavior c s Hostname
webGateway = webGateway_ <$> ask

-- | Fetch the gateway port to use.
webPort :: (AppCounter c, BehaviorState s) => Behavior c s Port
webPort = webPort_ <$> ask

-- | Fetch the system counters.
systemCounters :: (AppCounter c, BehaviorState s) =>
                  Behavior c s [TVar SystemCounter]
systemCounters = systemCounters_ <$> ask

-- | Fetch the app counter.
appCounter :: (AppCounter c, BehaviorState s) => Behavior c s (TVar c)
appCounter = appCounter_ <$> ask

-- | Sleep the requested number of seconds.
sleepSec :: (AppCounter c, BehaviorState s) => Int -> Behavior c s ()
sleepSec duration = sleepMsec $ duration * 1000

-- | Sleep the requested number of milliseconds.
sleepMsec :: (AppCounter c, BehaviorState s) => Int -> Behavior c s ()
sleepMsec duration = sleepUsec $ duration * 1000

-- | Sleep the requested number if microseconds.
sleepUsec :: (AppCounter c, BehaviorState s) => Int -> Behavior c s ()
sleepUsec duration = liftIO $ threadDelay duration
  
-- | Update the counter with the amount of bytes received.
receivedBytes :: (AppCounter c, BehaviorState s) =>
                 Int -> Behavior c s ()
receivedBytes amount = do
  systemCounters' <- systemCounters
  liftIO $ bulk (addBytesReceived amount) systemCounters'

-- | Specify an interval, e.g. a time interval.
interval :: (Int, Int) -> IO Int
interval = randomRIO

-- | Randomly select one of the elements in the provided list.
oneOfIO :: [a] -> IO a
oneOfIO xs = (xs !!) <$> randomRIO (0, length xs - 1)

-- | Randomly select one of the elements in the provided list. Adapted
-- for the Behavior monad.
oneOf :: (AppCounter c, BehaviorState s) => [a] -> Behavior c s a
oneOf = liftIO . oneOfIO
