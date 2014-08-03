module Simulation.Node.Counter
       ( Counter (..)
       ) where

import GHC.Int

class Counter a where
  empty       :: a
  addReceived :: Int -> a -> a
  getReceived :: a -> Int64

