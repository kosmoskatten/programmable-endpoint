module Simulation.Node.Counter
       ( Counter (access, modify)
       , DataSet (..)
       , ByteCounter (..)
       , create
       ) where

import Control.Concurrent.STM
import GHC.Int

data Counter a =
  Counter { access  :: IO a
          , modify  :: (a -> a) -> STM ()
          , dataSet :: TVar a }

class DataSet a where
  empty :: a

class ByteCounter a where
  addReceived :: Int -> a -> a
  getReceived :: a -> Int64

create :: (DataSet a, ByteCounter a) => IO (Counter a)
create = do
  dataSet' <- newTVarIO empty
  return $ Counter { access  = readTVarIO dataSet'
                   , modify  = modifyTVar' dataSet'
                   , dataSet = dataSet' }

instance Eq a => Eq (Counter a) where
  (==) x y = x == y

instance Show a => Show (Counter a) where
  show a = "Counter " ++ show a
