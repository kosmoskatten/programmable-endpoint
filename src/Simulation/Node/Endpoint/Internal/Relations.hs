{-# LANGUAGE OverloadedStrings #-}
module Simulation.Node.Endpoint.Internal.Relations
       ( Relations (..)
       , relations
       ) where

import Data.List (foldl')
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
relations = foldl' extract emptyRelations
  where
    extract :: Relations -> Tag BS.ByteString -> Relations
    extract acc (TagOpen "a" attr)      =
      maybe acc (\r -> acc {links = (r:links acc)}) $ lookup "href" attr

    extract acc (TagOpen "img" attr)    =
      maybe acc (\r -> acc {images = (r:images acc)}) $ lookup "src" attr

    extract acc (TagOpen "script" attr) =
      maybe acc (\r -> acc {scripts = (r:scripts acc)}) $ lookup "src" attr

    extract acc (TagOpen "link" attr)   =
      maybe acc (\r -> acc {stylesheets = (r:stylesheets acc)}) $
        lookup "href" attr
        
    extract  acc _                      = acc
