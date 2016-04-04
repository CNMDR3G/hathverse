{-# LANGUAGE OverloadedStrings #-}
module View.Homepage (
  homepage
) where

import Control.Monad (forM_)
import qualified Data.Text as T
import View.Common
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

homepage :: [(Int, T.Text)] -> Html
homepage idTitles = withTitleBody "home" $ do
  h1 "Problem Set"
  table ! class_ "table table-bordered table-hover" $ do
    thead . tr $ do
      th "#"
      th "Problem Title"
    tbody . forM_ idTitles $ \(pid, problemTitle) -> tr $ do
        th . toHtml . T.pack $ show pid
        td . toHtml $ problemTitle

