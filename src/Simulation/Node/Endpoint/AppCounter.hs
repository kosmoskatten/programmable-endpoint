-- | A type class for an extensible application counter. It will
-- operate on the behavior level only, with a separate instance per
-- behavior. It will not automatically aggregate on other levels,
-- i.e. node and endpoint level.
module Simulation.Node.Endpoint.AppCounter
       ( AppCounter (create)
       ) where

-- | An application defined counter.
class AppCounter a where
  -- | Create a new and empty counter instance.
  create :: a
