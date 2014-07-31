module Simulation.Node.Statistics
       ( Statistics
       , Counters (..)
       , create
       , counters
       , addBytesReceived
       , addBytesSent
       , atomically
       ) where

import Control.Concurrent.STM
  ( STM
  , TVar
  , atomically
  , newTVarIO
  , readTVarIO
  , modifyTVar )
import GHC.Int

newtype Statistics = Statistics (TVar Counters)

data Counters =
  Counters { bytesReceived :: !Int64
           , bytesSent     :: !Int64 }
  deriving Show

create :: IO Statistics
create = Statistics `fmap` newTVarIO empty

counters :: Statistics -> IO Counters
counters (Statistics c) = readTVarIO c

empty :: Counters
empty = Counters 0 0

addBytesReceived :: Int -> Statistics -> STM ()
addBytesReceived amount (Statistics c) =
  modifyTVar c $
    \c' -> c' { bytesReceived = bytesReceived c' + fromIntegral amount }

addBytesSent :: Int -> Statistics -> STM ()
addBytesSent amount (Statistics c) =
  modifyTVar c $
    \c' -> c' { bytesSent = bytesSent c' + fromIntegral amount }

instance Show Statistics where
  show _ = "Statistics (TVar Counters)"
