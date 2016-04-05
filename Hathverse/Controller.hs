module Hathverse.Controller where

import qualified Data.Text.Lazy as L
import Lucid
import Hathverse.Db
import Hathverse.View

homepage :: SqlPool -> IO L.Text
homepage pool =
  renderText . homepageView <$> allProblemIdTitles pool
