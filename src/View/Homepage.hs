{-# LANGUAGE OverloadedStrings #-}
module View.Homepage (
  homepage
) where

import Control.Monad (forM_)
import qualified Data.Text as T
import View.Common
import Lucid

homepage :: [(Int, T.Text)] -> Html ()
homepage idTitles = withTitleBody "home" $ do
  h1_ "Problem Set"
  table_ [class_ "table table-bordered table-hover"] $ do
    thead_ . tr_ $ do
      th_ "#"
      th_ "Problem Title"
    tbody_ . forM_ idTitles $ \(pid, problemTitle) -> tr_ $ do
        th_ . toHtml . T.pack $ show pid
        td_ . toHtml $ problemTitle

