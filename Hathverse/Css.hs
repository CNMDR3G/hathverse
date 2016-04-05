{-# LANGUAGE OverloadedStrings #-}
module Hathverse.Css where

import Prelude hiding (rem)
import Clay
import Data.Text.Lazy (Text)

defaultCss :: Text
defaultCss = render $ do
  body ? paddingTop (rem 5)
  nav ? backgroundColor "#392748"
