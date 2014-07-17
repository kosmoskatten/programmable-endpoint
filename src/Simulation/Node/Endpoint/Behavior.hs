{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simulation.Node.Endpoint.Behavior
       ( Behavior
       , BehaviorState
       , runBehavior
       , dummy1
       ) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (ReaderT, MonadIO, runReaderT, liftIO)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get, put)

newtype BehaviorT r s a =
  BehaviorT { extractBehaviorT :: ReaderT r (StateT s IO) a }
  deriving (Monad, MonadIO, MonadReader r, MonadState s)

type Behavior = BehaviorT Int

class BehaviorState a where
  initial :: a

runBehavior :: BehaviorState s => Behavior s () -> Int -> IO () 
runBehavior action rr = do
  _ <- runStateT (runReaderT (extractBehaviorT action) rr) initial
  return ()

data DummyState = DummyState !String
   deriving Show

instance BehaviorState DummyState where
  initial = DummyState "Frasse"

dummy1 :: Behavior DummyState ()
dummy1 = do
  myStatic <- ask
  myDynamic <- get
  put (DummyState "Sigge")
  myDynamic' <- get
  liftIO $ print myStatic
  liftIO $ print myDynamic
  liftIO $ print myDynamic'
  
