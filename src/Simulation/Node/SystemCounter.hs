-- | A system wide counter record which is instantiated on node,
-- endpoint and behavior level. The counter is aggregated upwards in
-- the hierarchy.
module Simulation.Node.SystemCounter
       ( SystemCounter (..)
       , create
       , addBytesReceived
       , addBytesSent
       , incActiveBehaviors
       , decActiveBehaviors
       , incBehaviorRestarts
       , bulk
       ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import GHC.Int

data SystemCounter =
  SystemCounter { bytesReceived    :: !Int64
                , bytesSent        :: !Int64
                , activeBehaviors  :: !Int64
                , behaviorRestarts :: !Int64 }
  deriving (Eq, Show)

-- | Create an empty system counter.
create :: SystemCounter
create = SystemCounter 0 0 0 0

-- | Add amount to the bytes received counter.
addBytesReceived :: Int -> SystemCounter -> SystemCounter
addBytesReceived amount counter =
  counter { bytesReceived = bytesReceived counter + fromIntegral amount }

-- | Add amount to the bytes sent counter.
addBytesSent :: Int -> SystemCounter -> SystemCounter
addBytesSent amount counter =
  counter { bytesSent = bytesSent counter + fromIntegral amount }

-- | Increase the active behaviors counter.
incActiveBehaviors :: SystemCounter -> SystemCounter
incActiveBehaviors counter =
  counter { activeBehaviors = activeBehaviors counter + 1 }

-- | Decrease the active behaviors counter.
decActiveBehaviors :: SystemCounter -> SystemCounter
decActiveBehaviors counter
  | activeBehaviors counter == 0 = counter
  | otherwise                    =
    counter { activeBehaviors = activeBehaviors counter - 1 }

incBehaviorRestarts :: SystemCounter -> SystemCounter
incBehaviorRestarts counter =
  counter { behaviorRestarts = behaviorRestarts counter + 1 }

-- | Apply a modifying action to all counters in the list.
bulk :: (SystemCounter -> SystemCounter) -> [TVar SystemCounter] -> IO ()
bulk modifier = atomically . mapM_ (`modifyTVar'` modifier)
