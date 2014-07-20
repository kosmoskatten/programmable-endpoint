module Simulation.Node.Endpoint
       ( Endpoint
       , Receipt
       , IpAddress
       , create
       , destroy
       , addBehavior
       , removeBehavior
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM
  ( TVar
  , atomically
  , newTVarIO
  , readTVar
  , modifyTVar
  , writeTVar
  )
import qualified Data.Map.Strict as Map
import Simulation.Node.Endpoint.Behavior
  ( Behavior
  , BehaviorState
  )

type IpAddress = String
type BehaviorMap = Map.Map Receipt Int

-- | An endpoint instance descriptor.
data Endpoint =
  Endpoint { ipAddress      :: !IpAddress
           , receiptCounter :: TVar Int
           , behaviorMap    :: TVar BehaviorMap
           }
  deriving Eq

-- | A receipt for an added behavior.
newtype Receipt = Receipt Int
  deriving (Eq, Ord)

-- | Create an endpoint instance.
create :: IpAddress -> IO Endpoint
create theIpAddress =
  Endpoint theIpAddress <$> newTVarIO 1 <*> newTVarIO (Map.empty)

-- | Destroy an endpoint instance.
destroy :: Endpoint -> IO ()
destroy _ = return ()

-- | Add a behavior to the endpoint.
addBehavior :: BehaviorState s => Behavior s a -> Endpoint -> IO Receipt
addBehavior action endpoint =
  atomically $ do
    let counter   = receiptCounter endpoint
        behaviors = behaviorMap endpoint
    receipt <- Receipt <$> readTVar counter
    modifyTVar counter (+ 1)
    modifyTVar behaviors (Map.insert receipt 1)
    return receipt        

-- | Remove a behavior from the endpoint.
removeBehavior :: Receipt -> Endpoint -> IO (Either String ())
removeBehavior receipt endpoint = do
  maybeBehavior <- atomically $ do
    behaviors <- readTVar (behaviorMap endpoint)
    case Map.lookup receipt behaviors of
      Just behavior -> do
        writeTVar (behaviorMap endpoint) (Map.delete receipt behaviors)
        return $ Just behavior
      _             -> return Nothing

  case maybeBehavior of
    Just behavior -> return $ Right ()
    _             -> return $ Left "No such behavior"
        


  
  
