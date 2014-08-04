{-# LANGUAGE OverloadedStrings #-}
module Simulation.Node.Service.Http.Server
       ( static
       , mkPrefixFunc
       , htmlResponse
       , loremIpsum
       ) where

import Simulation.Node.Service.Http
  ( HttpService
  , Response
  , basePrefix
  , selfStore
  , setResponseCode
  , setContentType
  , emptyResponse    
  )
import Snap.Util.FileServe (serveDirectory)
import qualified Text.Blaze.Html5 as H

-- | Helper service that will serve static files from the service's
-- self store.
static :: HttpService ()
static = serveDirectory =<< selfStore

-- | Helper service that is making a function to be used for creating
-- the correct prefix for links etc.
mkPrefixFunc :: HttpService (String -> H.AttributeValue)
mkPrefixFunc = appendTo `fmap` basePrefix
  where
    appendTo :: String -> String -> H.AttributeValue
    appendTo serviceBase resource = H.toValue $ serviceBase ++ resource

-- | Helper function that creates a response of type text/html and 200
-- OK.
htmlResponse :: Response
htmlResponse =
  setResponseCode 200 $
  setContentType "text/html" emptyResponse

-- | A chunk of text, 753 bytes long.
loremIpsum :: H.Html
loremIpsum =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit.\
    \Donec ac ligula at leo consequat volutpat.\
    \Duis egestas augue eu lorem aliquam lacinia.\
    \Curabitur condimentum eu eros ut tincidunt. Duis vitae turpis erat.\
    \Ut aliquam massa nec scelerisque congue.\
    \Nulla rhoncus odio est, vulputate fringilla lectus sodales sit amet.\
    \Donec nec pellentesque dui. Fusce congue quam eu posuere suscipit.\
    \Duis diam dolor, semper auctor luctus quis, condimentum eu est.\
    \Mauris lobortis aliquam velit, ac porta quam rhoncus id.\
    \Nullam at mattis erat.\
    \Aenean blandit augue tortor, quis euismod elit sodales in.\
    \Phasellus et condimentum libero, eu feugiat mauris.\
    \Etiam condimentum libero quis magna pellentesque ornare.\
    \Cras nunc libero, fermentum in eleifend tempor, blandit eu arcu."

