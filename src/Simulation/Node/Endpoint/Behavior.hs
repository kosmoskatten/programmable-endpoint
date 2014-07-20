{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simulation.Node.Endpoint.Behavior
       ( Behavior
       , BehaviorApiParam (..)
       , BehaviorState (..)
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

-- | The BehaviorT monad transformer; r is the api parameter type, s
-- is the user supplied behavior state and a is the reply type of the
-- action.
newtype BehaviorT r s a =
  BehaviorT { extractBehaviorT :: ReaderT r (StateT s IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader r, MonadState s)

-- | Type shortcut where the type r is forced to be of kind BehaviorApiParam.
type Behavior = BehaviorT BehaviorApiParam

-- | Record with api parameters for the execution of the Behavior monad.
data BehaviorApiParam =
  BehaviorApiParam { selfIpAddress_ :: !String }
  deriving Show

-- | Typeclass for the user supplied behavior state.
class BehaviorState a where
  -- Fetch the initial state and a slogan text for the behavior.
  fetch :: IO (Text, a)

-- | Run a behavior.
runBehavior :: BehaviorState s => Behavior s () -> BehaviorApiParam -> IO ()
runBehavior action param = do
  (_, initialState) <- fetch
  void $ runStateT (runReaderT (extractBehaviorT action) param) initialState

-- | Run a behavior in a way suitable for testing of behaviors.
runBehaviorTest :: BehaviorState s  =>
                   Behavior s a     ->
                   BehaviorApiParam -> IO (Text, a, s) 
runBehaviorTest action param = do
  (slogan, initialState) <- fetch
  (result, state)   <-
    runStateT (runReaderT (extractBehaviorT action) param) initialState
  return (slogan, result, state)

-- | Fetch own's ip address.
selfIpAddress :: Behavior s String
selfIpAddress = selfIpAddress_ <$> ask

-- | Sleep the requested number of seconds.
sleepSec :: Int -> Behavior s ()
sleepSec duration = sleepMsec $ duration * 1000

-- | Sleep the requested number of milliseconds.
sleepMsec :: Int -> Behavior s ()
sleepMsec duration = sleepUsec $ duration * 1000

-- | Sleep the requested number if microseconds.
sleepUsec :: Int -> Behavior s ()
sleepUsec duration = liftIO $ threadDelay duration
  
