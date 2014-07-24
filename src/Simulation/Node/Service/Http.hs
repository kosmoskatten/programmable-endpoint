{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simulation.Node.Service.Http
       ( HttpService
       , Routes (..)
       , as
       , toSnapRoutes
       , selfStore
       , module Snap.Test
       ) where

import Control.Applicative (Applicative, Alternative, (<$>))
import Control.Monad (MonadPlus)
import Control.Monad.Reader (ReaderT, MonadIO, runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.CatchIO (MonadCatchIO)
import qualified Data.ByteString.Char8 as BS
import Snap.Core
import Snap.Test
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
  HttpServiceApiParam { selfStore_ :: !FilePath }
  deriving Show

-- | A type describing a service's route mapping between an url and
-- a handler for the request.
newtype Routes a = Routes [ (BS.ByteString, HttpService a) ]

-- | A type describing an adjusted route mapping, ready to install.
newtype Installment a = Installment [ (BS.ByteString, HttpService a, FilePath) ]

-- | Convert a set of routes and a prefix to a proper installment.
as :: Routes a -> BS.ByteString -> Installment a
as (Routes xs) prefix =
  let prefix' = prefix `BS.snoc` '/'
  in Installment $
     map (\(url, action) ->
           (prefix' `BS.append` url, action, BS.unpack prefix')) xs

-- | Convert a list of installments to a list of proper snap
-- routes. The HttpService monad will be evaluated down to the snap
-- monad in this step.
toSnapRoutes :: [Installment a] -> [(BS.ByteString, Snap a)]
toSnapRoutes xs = concat $ map (\(Installment xs') -> map toSnap xs') xs

-- | Fetch own's self store position in the file system (relative to
-- current working directory).
selfStore :: HttpService FilePath
selfStore = selfStore_ <$> ask

-- | Run an HttpService in the Snap monad.
runHttpService :: HttpService a -> HttpServiceApiParam -> Snap a
runHttpService action param = runReaderT (extractHttpService action) param

toSnap :: (BS.ByteString, HttpService a, FilePath) -> (BS.ByteString, Snap a)
toSnap (url, action, filePath) = (url, runHttpService action makeParam)
  where
    makeParam :: HttpServiceApiParam
    makeParam = HttpServiceApiParam $ "httpServices" </> filePath