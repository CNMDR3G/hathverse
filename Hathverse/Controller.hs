{-# LANGUAGE DeriveGeneric #-}
module Hathverse.Controller where

import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.Aeson
import Control.Monad.Reader
import Lucid
import Hathverse.Db
import Hathverse.View
import Hathverse.Checker

homepage :: Query (Html ())
homepage = homepageView <$> allProblemIdTitles

problemPage :: Int64 -> Query (Html ())
problemPage pid = problemView pid <$> getProblemById pid

data CheckRequest = CheckRequest {
    probId :: Int64
  , solCode :: String
  } deriving Generic

instance FromJSON CheckRequest

data CheckResult = CheckResult {
    result :: String
  } deriving Generic

instance ToJSON CheckResult

checkApi :: CheckRequest -> Query CheckResult
checkApi (CheckRequest pid code) = do
  prob <- getProblemById pid
  case prob of
    Nothing -> return $ CheckResult "Problem not found."
    Just problem -> lift $ CheckResult <$> check problem code
