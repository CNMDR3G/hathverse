module Hathverse.Controller where

import qualified Data.Text.Lazy as L
import Lucid
import Hathverse.Db
import Hathverse.View
import Control.Monad.Reader

homepage :: ReaderT SqlPool IO L.Text
homepage = renderText . homepageView <$> allProblemIdTitles
