module Simulation.Node.Endpoint
       ( Endpoint
       , Receipt
       , IpAddress
       , create
       , reset
       , addBehavior
       , removeBehavior
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM
  ( TVar
  , atomically
  , newTVarIO
  , readTVar
  , modifyTVar
  , writeTVar
  )
import Control.Exception (AsyncException (..), handle, throwIO)
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Simulation.Node.Endpoint.Behavior
  ( Behavior
  , BehaviorState (..)
  , BehaviorApiParam (..)
  , runBehavior
  )

type IpAddress = String
type BehaviorMap = Map.Map Receipt (Async ())

-- | An endpoint instance descriptor.
data Endpoint =
  Endpoint { ipAddress      :: !IpAddress
           , receiptCounter :: TVar Int
           , behaviorMap    :: TVar BehaviorMap
           }
  deriving Eq

-- | A receipt for an added behavior.
newtype Receipt = Receipt Int
  deriving (Eq, Ord)

-- | Create an endpoint instance.
create :: IpAddress -> IO Endpoint
create theIpAddress =
  Endpoint theIpAddress <$> newTVarIO 1 <*> newTVarIO Map.empty

-- | Reset an endpoint instance by removing all behaviors.
reset :: Endpoint -> IO ()
reset endpoint = do
  behaviorList <- Map.toList <$> atomically (readTVar (behaviorMap endpoint))
  mapM_ (flip removeBehavior endpoint . fst) behaviorList

-- | Add a behavior to the endpoint.
addBehavior :: BehaviorState s => Behavior s () -> Endpoint -> IO Receipt
addBehavior action endpoint = do
  task <- async $ supervise action endpoint
  atomicallyAdd task
  where
    atomicallyAdd :: Async () -> IO Receipt
    atomicallyAdd task =
      atomically $ do
        let counter   = receiptCounter endpoint
            behaviors = behaviorMap endpoint
        receipt <- Receipt <$> readTVar counter
        modifyTVar counter (+ 1)
        modifyTVar behaviors (Map.insert receipt task)
        return receipt

-- | Supervise a behavior. If the behavior is crashed the behavior
-- shall be restarted by the supervisor. If the behavior is normally
-- terminated no action is taken.
supervise :: BehaviorState s => Behavior s () -> Endpoint -> IO ()
supervise action endpoint = do
  let apiParam = BehaviorApiParam (ipAddress endpoint)
  (_, initialState) <- fetch
  supervise' action apiParam initialState
  where
    supervise' :: BehaviorState s  =>
                  Behavior s ()    ->
                  BehaviorApiParam ->
                  s -> IO ()
    supervise' action' apiParam initialState = do
      task <- async $ runBehavior action' apiParam initialState
      terminate task `handle` do
        status <- waitCatch task
        case status of
          Left _ -> supervise' action' apiParam initialState
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

-- | Remove a behavior from the endpoint.
removeBehavior :: Receipt -> Endpoint -> IO (Either String ())
removeBehavior receipt endpoint = do
  maybeTask <- maybeAtomicallyDelete
  case maybeTask of
    Just task -> do
      cancel task
      void $ waitCatch task
      return $ Right ()
    _         -> return $ Left "Behavior was not found"
  where
    maybeAtomicallyDelete :: IO (Maybe (Async ()))
    maybeAtomicallyDelete =
      atomically $ do
        behaviors <- readTVar (behaviorMap endpoint)
        case Map.lookup receipt behaviors of
          entry@(Just _) -> do
            writeTVar (behaviorMap endpoint) (Map.delete receipt behaviors)
            return entry
          _              -> return Nothing
          

  
  
