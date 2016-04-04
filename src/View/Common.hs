{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}
module View.Common (
  withTitle
) where

import qualified Data.Text as T
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

withTitle :: _ -> Html -> Html
withTitle  pageTitle pageBody = docTypeHtml $ do
  H.head $ do
    H.title pageTitle
    let bootstrapCss = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
    link ! rel "stylesheet" ! type_ "text/css" ! href bootstrapCss
  pageBody
