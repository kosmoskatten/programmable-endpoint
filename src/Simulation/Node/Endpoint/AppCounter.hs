-- | A type class for an extensible application counter. It will
-- operate on the behavior level only, with a separate instance per
-- behavior. It will not automatically aggregate on other levels,
-- i.e. node and endpoint level.
module Simulation.Node.Endpoint.AppCounter
       ( BehaviorCounter (create)
       ) where

-- | A Behavior defined counter.
class BehaviorCounter a where
  -- | Create a new and empty counter instance.
  create :: a
