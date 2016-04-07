{-# LANGUAGE OverloadedStrings #-}
module Hathverse.View.Common where

import Lucid

headWithTitle :: Html () -> Html ()
headWithTitle pageTitle = head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ pageTitle
    link_ [rel_ "stylesheet", type_ "text/css", href_ bootstrapCss]
    link_ [rel_ "stylesheet", type_ "text/css", href_ codemirrorCss]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/default.css"]
    script_ [src_ codemirrorJs] ("" :: String)
    script_ [src_ codemirrorLangHs] ("" :: String)
  where bootstrapCss = "//cdn.bootcss.com/bootstrap/4.0.0-alpha.2/css/bootstrap.min.css"
        codemirrorJs = "//cdn.bootcss.com/codemirror/5.12.0/codemirror.min.js"
        codemirrorCss = "//cdn.bootcss.com/codemirror/5.12.0/codemirror.min.css"
        codemirrorLangHs = "//cdn.bootcss.com/codemirror/5.12.0/mode/haskell/haskell.min.js"

navigation :: Html ()
navigation =
  nav_ [class_ "navbar navbar-dark navbar-static-top"] $
    div_ [class_ "container"] $ do
      a_ [class_ "navbar-brand", href_ "/"] "Hathverse"
      ul_ [class_ "nav navbar-nav"] $
        li_ [class_ "nav-item"] $
          a_ [class_ "nav-link", href_ "/"] "home"

withTitleBody :: Html () -> Html () -> Html ()
withTitleBody pageTitle pageBody = doctypehtml_ $ do
    headWithTitle pageTitle
    body_ $ do
      navigation
      div_ [class_ "container"] pageBody
      script_ [src_ jquery] ("" :: String)
      script_ [src_ bootstrapJs] ("" :: String)
  where jquery = "//cdn.bootcss.com/jquery/1.11.3/jquery.min.js"
        bootstrapJs = "//cdn.bootcss.com/bootstrap/4.0.0-alpha.2/js/bootstrap.min.js"
