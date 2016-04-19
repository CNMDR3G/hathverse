{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hathverse.View.ProblemEdit where

import Data.Int (Int64)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Hathverse.View.Common
import Hathverse.Db (Problem(..))
import Lucid
import Text.HTML.SanitizeXSS

problemEditView :: Maybe (Int64, Problem) -> HtmlGen
problemEditView _ = errorView "TODO"
