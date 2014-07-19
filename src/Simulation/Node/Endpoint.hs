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
import Control.Concurrent.STM (TVar, newTVarIO)
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
  deriving Eq

-- | Create an endpoint instance.
create :: IpAddress -> IO Endpoint
create theIpAddress =
  Endpoint theIpAddress <$> newTVarIO 1 <*> newTVarIO (Map.empty)

-- | Destroy an endpoint instance.
destroy :: Endpoint -> IO ()
destroy _ = return ()

-- | Add a behavior to the endpoint.
addBehavior :: BehaviorState s => Behavior s a -> Endpoint -> IO Receipt
addBehavior action endpoint = return $ Receipt 1

-- | Remove a behavior from the endpoint.
removeBehavior :: Receipt -> Endpoint -> IO (Either String ())
removeBehavior _ _ = return $ Right ()


  
  
