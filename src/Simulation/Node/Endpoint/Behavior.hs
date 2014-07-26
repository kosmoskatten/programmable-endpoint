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
       , sleepSec
       , sleepMsec
       , sleepUsec
       ) where

import Control.Applicative (Applicative, (<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.Reader (ReaderT, MonadIO, runReaderT, liftIO)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get, put)
import Data.Text (Text)
import Network.Http.Client (Hostname, Port)

-- | The Behavior monad; s is the user supplied behavior
-- state and a is the reply type of the action.
newtype Behavior s a =
  Behavior { extractBehavior :: ReaderT BehaviorApiParam (StateT s IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader BehaviorApiParam, MonadState s )

-- | Record with api parameters for the execution of the Behavior monad.
data BehaviorApiParam =
  BehaviorApiParam { selfIpAddress_ :: !String
                   , webGateway_    :: !Hostname
                   , webPort_       :: !Port }
  deriving Show

-- | Typeclass for the user supplied behavior state.
class BehaviorState a where
  -- Fetch the initial state and a slogan text for the behavior.
  fetch :: IO (Text, a)

-- | Run a behavior.
runBehavior :: BehaviorState s  =>
               Behavior s ()    ->
               BehaviorApiParam ->
               s -> IO ()
runBehavior action param initialState =
  void $ runStateT (runReaderT (extractBehavior action) param) initialState

-- | Run a behavior in a way suitable for testing of behaviors.
runBehaviorTest :: BehaviorState s  =>
                   Behavior s a     ->
                   BehaviorApiParam -> IO (Text, a, s) 
runBehaviorTest action param = do
  (slogan, initialState) <- fetch
  (result, state)        <-
    runStateT (runReaderT (extractBehavior action) param) initialState
  return (slogan, result, state)

-- | Fetch own's ip address.
selfIpAddress :: BehaviorState s => Behavior s String
selfIpAddress = selfIpAddress_ <$> ask

-- | Sleep the requested number of seconds.
sleepSec :: BehaviorState s => Int -> Behavior s ()
sleepSec duration = sleepMsec $ duration * 1000

-- | Sleep the requested number of milliseconds.
sleepMsec :: BehaviorState s => Int -> Behavior s ()
sleepMsec duration = sleepUsec $ duration * 1000

-- | Sleep the requested number if microseconds.
sleepUsec :: BehaviorState s => Int -> Behavior s ()
sleepUsec duration = liftIO $ threadDelay duration
  
