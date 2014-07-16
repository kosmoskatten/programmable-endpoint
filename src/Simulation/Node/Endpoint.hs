module Simulation.Node.Endpoint
       ( Endpoint
       , IpAddress
       , create
       , destroy
       ) where

type IpAddress = String

-- | An endpoint instance descriptor.
data Endpoint =
  Endpoint { ipAddress_ :: !IpAddress }
  deriving Eq

-- | Create an endpoint instance.
create :: IpAddress -> IO Endpoint
create theIpAddress = return $ Endpoint theIpAddress

-- | Destroy an endpoint instance.
destroy :: Endpoint -> IO ()
destroy _ = return ()
