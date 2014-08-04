{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simulation.Node.Service.Http
       ( HttpService
       , Service
       , Routes (..)
       , activate
       , as
       , toSnapRoutes
       , selfStore
       , basePrefix
       , module Snap.Core
       ) where

import Control.Applicative (Applicative, Alternative, (<$>))
import Control.Monad (MonadPlus)
import Control.Monad.Reader (ReaderT, MonadIO, runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.CatchIO (MonadCatchIO)
import qualified Data.ByteString.Char8 as BS
import Snap.Core
import Snap.Http.Server
import System.FilePath ((</>))

-- | The HttpService monad.
newtype HttpService a =
  HttpService { extractHttpService :: ReaderT HttpServiceApiParam Snap a }
  deriving ( Functor, Applicative, Alternative, Monad
           , MonadPlus, MonadReader HttpServiceApiParam
           , MonadIO, MonadCatchIO, MonadSnap )

-- | Record with api parameters for the execution of the HttpService monad.
data HttpServiceApiParam =
  HttpServiceApiParam { selfStore_  :: !FilePath
                      , basePrefix_ :: !String}
  deriving Show

-- | A type describing a service's route mapping between an url and
-- a handler for the request.
newtype Routes a = Routes [ (BS.ByteString, HttpService a) ]

-- | A type describing a prepared service, ready to install.
newtype Service a = Service [ (BS.ByteString, HttpService a, String) ]

-- | Activate the http services, at the given port, in the current
-- thread.
activate :: Int -> [Service ()] -> IO ()
activate port services = do
  let config = setPort port defaultConfig
      routes = toSnapRoutes services
  httpServe config $ route routes

-- | Convert a set of routes and a prefix to a proper service.
as :: Routes a -> BS.ByteString -> Service a
as (Routes xs) prefix =
  let prefix' = prefix `BS.snoc` '/'
  in Service $
     map (\(url, action) ->
           (prefix' `BS.append` url, action, BS.unpack prefix')) xs

-- | Convert a list of installments to a list of proper snap
-- routes. The HttpService monad will be evaluated down to the snap
-- monad in this step.
toSnapRoutes :: [Service a] -> [(BS.ByteString, Snap a)]
toSnapRoutes = concatMap (\(Service xs') -> map toSnap xs')

-- | Fetch own's self store position in the file system (relative to
-- current working directory). E.g. if the service's name is foo the
-- selfStore will return httpServices/foo/ as the directory where
-- static data for the service can be stored.
selfStore :: HttpService FilePath
selfStore = selfStore_ <$> ask

-- | Fetch own's base prefix url. This is to be used for any kind of
-- linking to resources inside the own service to that the service
-- name always become the prefix in the url.
basePrefix :: HttpService String
basePrefix = basePrefix_ <$> ask

-- | Run an HttpService in the Snap monad.
runHttpService :: HttpService a -> HttpServiceApiParam -> Snap a
runHttpService action = runReaderT (extractHttpService action)

toSnap :: (BS.ByteString, HttpService a, String) -> (BS.ByteString, Snap a)
toSnap (url, action, prefix) = (url, runHttpService action makeParam)
  where
    makeParam :: HttpServiceApiParam
    makeParam = HttpServiceApiParam ("httpServices" </> prefix)
                                    ('/':prefix)
