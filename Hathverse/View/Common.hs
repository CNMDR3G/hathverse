{-# LANGUAGE OverloadedStrings #-}
module Hathverse.View.Common where

import Lucid
import Data.Int (Int64)
import Control.Monad.Reader
import Hathverse.Db (User(..))
import Data.Text (Text)
import Data.Monoid

type HtmlGen = HtmlT (Reader (Maybe (Int64, User))) ()

bootstrapCdnSite :: Text
bootstrapCdnSite = "//cdn.bootcss.com"

bootstrapCss
  , bootstrapJs
  , jqueryJs
  , codemirrorCss
  , codemirrorJs
  , codemirrorLangHs
  :: Text

bootstrapCss = bootstrapCdnSite <> "/bootstrap/4.0.0-alpha.2/css/bootstrap.min.css"
bootstrapJs = bootstrapCdnSite <> "/bootstrap/4.0.0-alpha.2/js/bootstrap.min.js"
codemirrorCss = bootstrapCdnSite <> "/codemirror/5.12.0/codemirror.min.css"
jqueryJs = bootstrapCdnSite <> "/jquery/1.11.3/jquery.min.js"
codemirrorJs = bootstrapCdnSite <> "/codemirror/5.12.0/codemirror.min.js"
codemirrorLangHs = bootstrapCdnSite <> "/codemirror/5.12.0/mode/haskell/haskell.min.js"

headWithTitle :: Monad m => HtmlT m () -> HtmlT m ()
headWithTitle pageTitle = head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ pageTitle
    link_ [rel_ "stylesheet", type_ "text/css", href_ bootstrapCss]
    link_ [rel_ "stylesheet", type_ "text/css", href_ codemirrorCss]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/default.css"]
    script_ [src_ jqueryJs] ("" :: String)
    script_ [src_ codemirrorJs] ("" :: String)
    script_ [src_ codemirrorLangHs] ("" :: String)

navigation :: HtmlGen
navigation =
  nav_ [class_ "navbar navbar-dark navbar-static-top"] $
    div_ [class_ "container"] $ do
      a_ [class_ "navbar-brand", href_ "/"] "Hathverse"
      ul_ [class_ "nav navbar-nav", style_ "float: right"] $ do
        maybeUser <- lift ask
        case maybeUser of
          Just (_, user) -> do
            li_ [class_ "nav-item"] $
              a_ [class_ "nav-link", href_ "/problems/new"] "contribute"
            li_ [class_ "nav-item"] $
              a_ [class_ "nav-link", href_ "#"] $ toHtml $ userName user
            li_ [class_ "nav-item"] $
              a_ [class_ "nav-link", href_ "/logout"] "logout"
          Nothing ->
            li_ [class_ "nav-item"] $
              a_ [class_ "nav-link", href_ "/login"] "login"

withTitleBody :: HtmlGen -> HtmlGen -> HtmlGen
withTitleBody pageTitle pageBody = doctypehtml_ $ do
    headWithTitle pageTitle
    body_ $ do
      navigation
      div_ [class_ "container"] pageBody
      script_ [src_ bootstrapJs] ("" :: String)

errorView :: HtmlGen -> HtmlGen
errorView msg = withTitleBody "Error" $ h1_ msg
