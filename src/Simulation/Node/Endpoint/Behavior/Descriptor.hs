module Simulation.Node.Endpoint.Behavior.Descriptor
       ( Descriptor (..)
       ) where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TVar)
import Data.Text (Text)
import Simulation.Node.SystemCounter (SystemCounter)

-- | A descriptor for an installed, active, behavior.
data Descriptor c =
  Descriptor { slogan        :: !Text
             , counter       :: TVar SystemCounter
             , appCounter    :: TVar c
             , thread        :: Async () }
  deriving Eq

instance Show c => Show (Descriptor c) where
  show desc = "BehaviorDesc { theSlogan = " ++ show (slogan desc) ++ "}"
