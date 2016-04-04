{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}
module View.Common where

import Prelude hiding (div)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

headWithTitle :: Html -> Html
headWithTitle pageTitle = H.head $ do
    meta ! charset "utf-8"
    meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.title pageTitle
    link ! rel "stylesheet" ! type_ "text/css" ! href bootstrapCss
    link ! rel "stylesheet" ! type_ "text/css" ! href "/css/default.css"
  where bootstrapCss = "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.2/css/bootstrap.min.css"

navigation :: Html
navigation =
  nav ! class_ "navbar navbar-dark navbar-fixed-top" $
    div ! class_ "container" $ do
      a ! class_ "navbar-brand" ! href "/" $ "Hathverse"
      ul ! class_ "nav navbar-nav" $
        li ! class_ "nav-item" $
          a ! class_ "nav-link" ! href "/" $ "home"

withTitleBody :: Html -> Html -> Html
withTitleBody pageTitle pageBody = docTypeHtml $ do
    headWithTitle pageTitle
    H.body $ do
      navigation
      div ! class_ "container" $ pageBody
      script ! src jquery $ ""
      script ! src bootstrapJs $ ""
  where jquery = "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
        bootstrapJs = "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.2/js/bootstrap.min.js"
