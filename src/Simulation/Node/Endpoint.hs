module Simulation.Node.Endpoint
       ( Endpoint (behaviors, counter)
       , IpAddress
       , Simulation.Node.Endpoint.create
       , reset
       , addBehavior
       , removeBehavior
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM
  ( STM
  , TVar
  , atomically
  , newTVarIO
  , readTVar
  , readTVarIO
  , modifyTVar'
  , writeTVar
  )
import Control.Exception (AsyncException (..), handle, throwIO)
import Control.Monad (void, when)
import Data.List (delete)
import Network.Socket (SockAddr)
import Network.Http.Client (toSockAddrIPv4)
import Simulation.Node.SystemCounter
  ( SystemCounter
  , bulk
  , incActiveBehaviors
  , decActiveBehaviors
  , incBehaviorRestarts
  )
import qualified Simulation.Node.SystemCounter as SC
import Simulation.Node.Endpoint.AppCounter
import qualified Simulation.Node.Endpoint.AppCounter as AC
import Simulation.Node.Endpoint.Behavior
  ( Behavior
  , BehaviorState (..)
  , BehaviorApiParam (..)
  , Hostname
  , Port
  , runBehavior
  )
import Simulation.Node.Endpoint.Behavior.Descriptor (Descriptor (..))
import System.IO (stderr, hPrint)

type IpAddress = String

-- | An endpoint instance descriptor.
data Endpoint c =
  Endpoint { webGateway  :: !Hostname
           , webPort     :: !Port
           , nodeCounter :: TVar SystemCounter             
           , behaviors   :: TVar [Descriptor c]
           , counter     :: TVar SystemCounter
           , ipAddress   :: !SockAddr
           }
  deriving Eq

-- | Create an endpoint instance.
create :: AppCounter c =>
          IpAddress -> Hostname -> Port -> TVar SystemCounter-> IO (Endpoint c)
create ip gateway port nodeCounter' =
  Endpoint gateway port nodeCounter' <$> newTVarIO []
                                     <*> newTVarIO SC.create
                                     <*> toSockAddrIPv4 ip

-- | Reset an endpoint instance by removing all behaviors.
reset :: (Eq c, AppCounter c) => Endpoint c -> IO ()
reset endpoint = do
  behaviors' <- readTVarIO (behaviors endpoint)
  mapM_ (removeBehavior endpoint) behaviors'

-- | Add a behavior to the endpoint.
addBehavior :: (AppCounter c, BehaviorState s) =>
               Behavior c s () -> Endpoint c -> IO (Descriptor c)
addBehavior action endpoint = do
  (theSlogan, state) <- fetch
  behaviorCounter    <- newTVarIO SC.create
  theAppCounter      <- newTVarIO AC.create
  let params = BehaviorApiParam (ipAddress endpoint)
                                (webGateway endpoint)
                                (webPort endpoint)
                                [ nodeCounter endpoint
                                , Simulation.Node.Endpoint.counter endpoint
                                , behaviorCounter ]
                                theAppCounter
  theThread <- async $ supervise action params state
  let behaviorDesc =
        Descriptor theSlogan behaviorCounter theAppCounter theThread
  atomically $ modifyTVar' (behaviors endpoint) (behaviorDesc:)
  return behaviorDesc
               
-- | Supervise a behavior. If the behavior is crashed the behavior
-- shall be restarted by the supervisor. If the behavior is normally
-- terminated no action is taken.
supervise :: (AppCounter c, BehaviorState s) =>
              Behavior c s () -> BehaviorApiParam c -> s -> IO ()
supervise action params state = do
  task <- async $ runBehavior action params state
  bulk incActiveBehaviors (systemCounters_ params)
  terminate task `handle` do
    status <- waitCatch task
    bulk decActiveBehaviors (systemCounters_ params)
    case status of
      Left cause -> do
        hPrint stderr cause
        bulk incBehaviorRestarts (systemCounters_ params)
        supervise action params state
      _      -> return ()

-- | Terminate is called in case the supervisor is cancelled. This
-- to make sure that the behavior is properly terminated.
terminate :: Async () -> AsyncException -> IO ()
terminate task e =
  case e of
    ThreadKilled -> do
      cancel task
      void $ waitCatch task
    _            -> throwIO e

-- | Remove and terminate a behavior from the endpoint.
removeBehavior :: (Eq c, AppCounter c) => Endpoint c -> Descriptor c -> IO ()
removeBehavior endpoint behavior = do
  isDeleted <- atomically $ maybeDelete behavior (behaviors endpoint)
  when isDeleted $ do
    cancel (thread behavior)
    void $ waitCatch (thread behavior)

maybeDelete :: (Eq c, AppCounter c) =>
               Descriptor c -> TVar [Descriptor c] -> STM Bool
maybeDelete behavior behaviors' = do
  xs <- readTVar behaviors'
  if behavior `elem` xs then do
    writeTVar behaviors' (behavior `delete` xs)
    return True
    else return False

