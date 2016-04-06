module Hathverse.Controller where

import Data.Int (Int64)
import qualified Data.Text.Lazy as L
import Lucid
import Hathverse.Db
import Hathverse.View
import Control.Monad.Reader

homepage :: Query L.Text
homepage = renderText . homepageView <$> allProblemIdTitles

problemPage :: Int64 -> Query L.Text
problemPage  pid = renderText . problemView <$> getProblemById pid
