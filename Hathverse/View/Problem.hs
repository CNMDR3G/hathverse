{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hathverse.View.Problem where

import Data.Int (Int64)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Hathverse.View.Common
import Hathverse.Db (Problem(..))
import Lucid

problemView :: Int64 -> Maybe Problem -> HtmlGen
problemView _ Nothing = withTitleBody "404" $ h1_ "Not found."
problemView pid (Just Problem{..}) =
  let idTitle = toHtml $ "#" <> T.pack (show pid) <> " " <> problemTitle
  in  withTitleBody idTitle $ do
        h1_ idTitle
        div_ [id_ "description"] $ toHtml problemDescription
        div_ [id_ "code"] $ toHtml problemTemplate
        div_ [id_ "buttons"] $
          button_ [id_ "run", class_ "btn btn-primary"] $ toHtml ("run" :: String)
        div_ [id_ "results"] $
          pre_ [id_ "result"] $ toHtml ("" :: String)
        script_ [src_ "/js/problem.js"] ("" :: String)
