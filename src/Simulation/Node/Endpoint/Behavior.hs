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
       , sleepSec
       , sleepMsec
       , sleepUsec
       , updateBytesReceived         
       , interval
       , oneOfIO
       , oneOf  
       ) where

import Control.Applicative (Applicative, (<$>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar')
import Control.Monad (void)
import Control.Monad.Reader (ReaderT, MonadIO, runReaderT, liftIO)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get, put)
import Data.Text (Text)
import Network.Http.Client (Hostname, Port)
import Simulation.Node.Counter (Counter (..))
import System.Random (randomRIO)

-- | The Behavior monad; c is the counter type, s is the user supplied
-- behavior state and a is the reply type of the action.
newtype Behavior c s a =
  Behavior { extractBehavior :: ReaderT (BehaviorApiParam c) (StateT s IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader (BehaviorApiParam c), MonadState s )

-- | Record with api parameters for the execution of the Behavior monad.
data BehaviorApiParam c =
  BehaviorApiParam { selfIpAddress_ :: !String
                   , webGateway_    :: !Hostname
                   , webPort_       :: !Port
                   , counters_      :: ![TVar c] }

-- | Typeclass for the user supplied behavior state.
class BehaviorState a where
  -- Fetch the initial state and a slogan text for the behavior.
  fetch :: IO (Text, a)

-- | Run a behavior.
runBehavior :: (Counter c, BehaviorState s)  =>
               Behavior c s ()    ->
               BehaviorApiParam c ->
               s                  -> IO ()
runBehavior action param initialState =
  void $ runStateT (runReaderT (extractBehavior action) param) initialState

-- | Run a behavior in a way suitable for testing of behaviors.
runBehaviorTest :: (Counter c, BehaviorState s)  =>
                   Behavior c s a     ->
                   BehaviorApiParam c -> IO (Text, a, s) 
runBehaviorTest action param = do
  (slogan, initialState) <- fetch
  (result, state)        <-
    runStateT (runReaderT (extractBehavior action) param) initialState
  return (slogan, result, state)

-- | Fetch own's ip address.
selfIpAddress :: (Counter c, BehaviorState s) => Behavior c s String
selfIpAddress = selfIpAddress_ <$> ask

-- | Fetch the gateway ip address to use.
webGateway :: (Counter c, BehaviorState s) => Behavior c s Hostname
webGateway = webGateway_ <$> ask

-- | Fetch the gateway port to use.
webPort :: (Counter c, BehaviorState s) => Behavior c s Port
webPort = webPort_ <$> ask

-- | Sleep the requested number of seconds.
sleepSec :: (Counter c, BehaviorState s) => Int -> Behavior c s ()
sleepSec duration = sleepMsec $ duration * 1000

-- | Sleep the requested number of milliseconds.
sleepMsec :: (Counter c, BehaviorState s) => Int -> Behavior c s ()
sleepMsec duration = sleepUsec $ duration * 1000

-- | Sleep the requested number if microseconds.
sleepUsec :: (Counter c, BehaviorState s) => Int -> Behavior c s ()
sleepUsec duration = liftIO $ threadDelay duration
  
-- | Update the counter with the amount of bytes received.
updateBytesReceived :: (Counter c, BehaviorState s) =>
                       Int -> Behavior c s ()
updateBytesReceived amount = do
  counters <- counters_ <$> ask
  liftIO (atomically $ mapM_ updateOneCounter counters)
  where
    updateOneCounter :: (Counter c) => TVar c -> STM ()
    updateOneCounter counter = modifyTVar' counter $ addReceived amount

-- | Specify an interval, e.g. a time interval.
interval :: (Int, Int) -> IO Int
interval = randomRIO

-- | Randomly select one of the elements in the provided list.
oneOfIO :: [a] -> IO a
oneOfIO xs = (xs !!) <$> randomRIO (0, length xs - 1)

-- | Randomly select one of the elements in the provided list. Adapted
-- for the Behavior monad.
oneOf :: (BehaviorState s, Counter c) => [a] -> Behavior c s a
oneOf = liftIO . oneOfIO