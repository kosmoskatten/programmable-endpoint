module Simulation.Node.Endpoint
       ( Endpoint (epCounter)
       , BehaviorDesc (theSlogan, theSystemCounter, theAppCounter)
       , IpAddress
       , Simulation.Node.Endpoint.create
       , reset
       , addBehavior
       , removeBehavior
       , listAll
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
import Data.Text (Text)
import Network.Socket (SockAddr)
import Network.Http.Client (toSockAddrIPv4)
import Simulation.Node.SystemCounter
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
import System.IO (stderr, hPrint)

type IpAddress = String

-- | A descriptor of an installed behavior.
data BehaviorDesc c =
  BehaviorDesc { theSlogan        :: !Text
               , theSystemCounter :: TVar SystemCounter
               , theAppCounter    :: TVar c
               , theThread        :: Async () }
  deriving Eq

instance Show c => Show (BehaviorDesc c) where
  show desc = "BehaviorDesc { theSlogan = " ++ show (theSlogan desc) ++ "}"

-- | An endpoint instance descriptor.
data Endpoint c =
  Endpoint { webGateway     :: !Hostname
           , webPort        :: !Port
           , nodeCounter    :: TVar SystemCounter             
           , behaviors      :: TVar [BehaviorDesc c]
           , epCounter      :: TVar SystemCounter
           , ipAddress      :: !SockAddr
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
               Behavior c s () -> Endpoint c -> IO (BehaviorDesc c)
addBehavior action endpoint = do
  (slogan, state) <- fetch
  behaviorCounter <- newTVarIO SC.create
  appCounter      <- newTVarIO AC.create
  let params = BehaviorApiParam (ipAddress endpoint)
                                (webGateway endpoint)
                                (webPort endpoint)
                                [ nodeCounter endpoint
                                , epCounter endpoint
                                , behaviorCounter ]
                                appCounter
  thread <- async $ supervise action params state
  let behaviorDesc = BehaviorDesc slogan behaviorCounter appCounter thread
  atomically $ modifyTVar' (behaviors endpoint) (behaviorDesc:)
  return behaviorDesc
               
-- | Supervise a behavior. If the behavior is crashed the behavior
-- shall be restarted by the supervisor. If the behavior is normally
-- terminated no action is taken.
supervise :: (AppCounter c, BehaviorState s) =>
              Behavior c s () -> BehaviorApiParam c -> s -> IO ()
supervise action params state = do
  task <- async $ runBehavior action params state
  terminate task `handle` do
    status <- waitCatch task
    case status of
      Left cause -> do
        hPrint stderr cause
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
removeBehavior :: (Eq c, AppCounter c) => Endpoint c -> BehaviorDesc c -> IO ()
removeBehavior endpoint behavior = do
  isDeleted <- atomically $ maybeDelete behavior (behaviors endpoint)
  when isDeleted $ do
    cancel (theThread behavior)
    void $ waitCatch (theThread behavior)

maybeDelete :: (Eq c, AppCounter c) =>
               BehaviorDesc c -> TVar [BehaviorDesc c] -> STM Bool
maybeDelete behavior behaviors' = do
  xs <- readTVar behaviors'
  if behavior `elem` xs then do
    writeTVar behaviors' (behavior `delete` xs)
    return True
    else return False
                
-- List all behavior descriptors.
listAll :: Endpoint c -> IO [BehaviorDesc c]
listAll endpoint = readTVarIO (behaviors endpoint)

