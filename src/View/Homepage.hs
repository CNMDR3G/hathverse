{-# LANGUAGE OverloadedStrings #-}
module View.Homepage (
  homepage
) where

import View.Common
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

homepage :: Html
homepage = withTitle "home" $ do
    H.form ! method "post" ! action "/check" $ do
      textarea ! name "solution" $
        "main = putStrLn \"Hello world!\""
      H.input ! type_ "submit" ! value "submit"
