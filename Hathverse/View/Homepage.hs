{-# LANGUAGE OverloadedStrings #-}
module Hathverse.View.Homepage where

import Data.Int (Int64)
import Data.Monoid ((<>))
import Control.Monad (forM_)
import qualified Data.Text as T
import Hathverse.View.Common
import Lucid

homepageView :: [(Int64, T.Text)] -> HtmlGen
homepageView idTitles = withTitleBody "home" $ do
  h1_ "Problem Set"
  table_ [class_ "table table-bordered table-hover"] $ do
    thead_ . tr_ $
      th_ "#" >> th_ "Problem Title"
    tbody_ . forM_ idTitles $ \(pid, problemTitle) -> tr_ $ do
        th_ . toHtml . T.pack $ show pid
        td_ $ a_ [href_ $ "/problems/" <> T.pack (show pid)] $ toHtml problemTitle

