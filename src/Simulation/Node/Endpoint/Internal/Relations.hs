{-# LANGUAGE OverloadedStrings #-}
module Simulation.Node.Endpoint.Internal.Relations
       ( Relations (..)
       , relations
       ) where

import qualified Data.ByteString.Char8 as BS
import Text.HTML.TagSoup (Tag (..))

data Relations =
  Relations { links       :: ![BS.ByteString]
            , images      :: ![BS.ByteString]
            , scripts     :: ![BS.ByteString]
            , stylesheets :: ![BS.ByteString] }
  deriving Show

emptyRelations :: Relations
emptyRelations = Relations [] [] [] []
              
-- | From the lists of tags, extract relations document.
relations :: [Tag BS.ByteString] -> Relations
relations = foldr extract emptyRelations
  where
    extract :: Tag BS.ByteString -> Relations -> Relations
    extract (TagOpen "a" attr) acc      =
      maybe acc (\r -> acc {links = (r:links acc)}) $ lookup "href" attr

    extract (TagOpen "img" attr) acc    =
      maybe acc (\r -> acc {images = (r:images acc)}) $ lookup "src" attr

    extract (TagOpen "script" attr) acc =
      maybe acc (\r -> acc {scripts = (r:scripts acc)}) $ lookup "src" attr

    extract (TagOpen "link" attr) acc   =
      maybe acc (\r -> acc {stylesheets = (r:stylesheets acc)}) $
        lookup "href" attr
        
    extract _ acc                       = acc
