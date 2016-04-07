{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hathverse.View.Problem where

import Data.Int (Int64)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Hathverse.View.Common
import Hathverse.Db (Problem(..))
import Lucid

problemView :: Maybe (Int64, Problem) -> Html ()
problemView Nothing = withTitleBody "404" $ h1_ "Not found."
problemView (Just (pid, Problem{..})) =
  let idTitle = toHtml $ "#" <> T.pack (show pid) <> " " <> problemTitle
  in  withTitleBody idTitle $ do
        h1_ idTitle
        div_ [id_ "description"] $ toHtml problemDescription
        div_ [id_ "code"] $ toHtml problemTemplate
        script_ [src_ "/js/problem.js"] ("" :: String)
