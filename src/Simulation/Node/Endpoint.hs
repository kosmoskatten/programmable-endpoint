module Simulation.Node.Endpoint
       ( Endpoint (counter)
       , Receipt
       , IpAddress
       , Simulation.Node.Endpoint.create
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
import Simulation.Node.Counter
import qualified Simulation.Node.Counter as Counter
import Simulation.Node.Endpoint.Behavior
  ( Behavior
  , BehaviorState (..)
  , BehaviorApiParam (..)
  , Hostname
  , Port
  , runBehavior
  )
import System.IO

type IpAddress = String
type BehaviorMap = Map.Map Receipt (Async ())

-- | An endpoint instance descriptor.
data Endpoint c =
  Endpoint { ipAddress      :: !IpAddress
           , webGateway     :: !Hostname
           , webPort        :: !Port
           , receiptCounter :: TVar Int
           , behaviorMap    :: TVar BehaviorMap
           , counter        :: Counter c
           }
  deriving Eq

-- | A receipt for an added behavior.
newtype Receipt = Receipt Int
  deriving (Eq, Ord)

-- | Create an endpoint instance.
create :: (DataSet a, ByteCounter a) =>
          IpAddress -> Hostname -> Port -> IO (Endpoint a)
create ip gateway port =
  Endpoint ip gateway port <$> newTVarIO 1
                           <*> newTVarIO Map.empty
                           <*> Counter.create

-- | Reset an endpoint instance by removing all behaviors.
reset :: (DataSet a, ByteCounter a) => Endpoint a -> IO ()
reset endpoint = do
  behaviorList <- Map.toList <$> atomically (readTVar (behaviorMap endpoint))
  mapM_ (flip removeBehavior endpoint . fst) behaviorList

-- | Add a behavior to the endpoint.
addBehavior :: (DataSet c, ByteCounter c, BehaviorState s) =>
               Behavior c s () -> Endpoint c -> IO Receipt
addBehavior action endpoint = do
  task <- async $ supervise action endpoint
  atomicallyAdd task
  where
    atomicallyAdd :: Async () -> IO Receipt
    atomicallyAdd task =
      atomically $ do
        let receiptCounter'   = receiptCounter endpoint
            behaviors         = behaviorMap endpoint
        receipt <- Receipt <$> readTVar receiptCounter'
        modifyTVar receiptCounter' (+ 1)
        modifyTVar behaviors (Map.insert receipt task)
        return receipt

-- | Supervise a behavior. If the behavior is crashed the behavior
-- shall be restarted by the supervisor. If the behavior is normally
-- terminated no action is taken.
supervise :: (DataSet c, ByteCounter c, BehaviorState s) =>
             Behavior c s () -> Endpoint c -> IO ()
supervise action endpoint = do
  behaviorCounter <- Counter.create
  let apiParam = BehaviorApiParam (ipAddress endpoint)
                                  (webGateway endpoint)
                                  (webPort endpoint)
                                  [counter endpoint, behaviorCounter]
  (_, initialState) <- fetch
  supervise' action apiParam initialState
  where
    supervise' :: (DataSet c, ByteCounter c, BehaviorState s) =>
                  Behavior c s ()    ->
                  BehaviorApiParam c ->
                  s -> IO ()
    supervise' action' apiParam initialState = do
      task <- async $ runBehavior action' apiParam initialState
      terminate task `handle` do
        status <- waitCatch task
        case status of
          Left cause -> do
            hPutStrLn stderr (show cause)
            supervise' action' apiParam initialState
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
removeBehavior :: (DataSet c, ByteCounter c) =>
                  Receipt -> Endpoint c -> IO (Either String ())
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
          

  
  
