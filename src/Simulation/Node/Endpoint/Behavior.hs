{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simulation.Node.Endpoint.Behavior
       ( Behavior
       , BehaviorApiParam (..)
       , BehaviorState (..)
       , runBehaviorTest
       , get
       , put
       , liftIO
       , selfIpAddress
       ) where

import Control.Applicative (Applicative, (<$>))
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

-- | Run a behavior in a way suitable for testing of behaviors.
runBehaviorTest :: BehaviorState s  =>
                   Behavior s a     ->
                   BehaviorApiParam -> IO (Text, a, s) 
runBehaviorTest action param = do
  (slogan, initial) <- fetch
  (result, state)   <-
    runStateT (runReaderT (extractBehaviorT action) param) initial
  return (slogan, result, state)

-- | Fetch own's ip address.
selfIpAddress :: Behavior s String
selfIpAddress = selfIpAddress_ <$> ask
  
