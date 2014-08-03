{-# LANGUAGE OverloadedStrings #-}
module Simulation.Node.Endpoint.Internal.Relations
       ( Relations (..)
       , relations
       ) where

import Data.List (foldl')
import Data.Text (Text)
import Text.HTML.TagSoup (Tag (..))

data Relations =
  Relations { links       :: ![Text]
            , images      :: ![Text]
            , scripts     :: ![Text]
            , stylesheets :: ![Text] }
  deriving Show

emptyRelations :: Relations
emptyRelations = Relations [] [] [] []
              
-- | From the lists of tags, extract relations document.
relations :: [Tag Text] -> Relations
relations = foldl' extract emptyRelations
  where
    extract :: Relations -> Tag Text -> Relations
    extract acc (TagOpen "a" attr)      =
      maybe acc (\r -> acc {links = r:links acc}) $ lookup "href" attr

    extract acc (TagOpen "img" attr)    =
      maybe acc (\r -> acc {images = r:images acc}) $ lookup "src" attr

    extract acc (TagOpen "script" attr) =
      maybe acc (\r -> acc {scripts = r:scripts acc}) $ lookup "src" attr

    extract acc (TagOpen "link" attr)   =
      maybe acc (\r -> acc {stylesheets = r:stylesheets acc}) $
        lookup "href" attr
        
    extract  acc _                      = acc
