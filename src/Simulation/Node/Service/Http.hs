{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simulation.Node.Service.Http where

import Control.Applicative (Applicative, Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Reader (ReaderT, MonadIO, runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.CatchIO (MonadCatchIO)
import qualified Data.ByteString.Char8 as BS
import Snap.Core
import Snap.Http.Server

-- | The HttpService monad.
newtype HttpService a =
  HttpService { extractHttpService :: ReaderT HttpServiceApiParam Snap a }
  deriving ( Functor, Applicative, Alternative, Monad
           , MonadPlus, MonadReader HttpServiceApiParam
           , MonadIO, MonadCatchIO, MonadSnap )

-- | Record with api parameters for the execution of the HttpService monad.
data HttpServiceApiParam = HttpServiceApiParam
  deriving Show

-- | A type describing a service's route mapping between an url and
-- a handler for the request.
newtype Routes = Routes [ (BS.ByteString, HttpService ()) ]

-- | A type describing an adjusted route mapping, ready to install.
newtype Installment = Installment [ (BS.ByteString, HttpService ()) ]

-- | Run an HttpService in the Snap monad.
runHttpService :: HttpService a -> HttpServiceApiParam -> Snap a
runHttpService action param = runReaderT (extractHttpService action) param

-- | Convert a set of routes and a prefix to a proper installment.
as :: Routes -> BS.ByteString -> Installment
as (Routes xs) prefix =
  let prefix' = prefix `BS.snoc` '/'
  in Installment $ map (\(url, action) -> (prefix' `BS.append` url, action)) xs

toSnapRoutes :: [Installment]
                -> HttpServiceApiParam
                -> [(BS.ByteString, Snap ())]
toSnapRoutes xs param =
  concat $ map (\(Installment xs') -> map (toSnap param) xs') xs

toSnap :: HttpServiceApiParam
          -> (BS.ByteString, HttpService ())
          -> (BS.ByteString, Snap ())
toSnap param (url, action) = (url, runHttpService action param)
