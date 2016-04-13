{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Hathverse.View.Login where

import Hathverse.View.Common
import Lucid

loginView :: HtmlGen
loginView = withTitleBody "login" $ do

        div_ [class_ "login-form"] $ do

          div_ [class_ "alert alert-danger", role_ "alert", id_ "err-msg", style_ "display: none"] ""

          form_ [action_ "#"] $ do

            div_ [class_ "form-group row"] $ do
              _ <- label_ [class_ "col-sm-3 form-control-label", for_ "username"] "Username:"
              div_ [class_ "col-sm-9"] $
                input_ [id_ "username", class_ "form-control", type_ "text"]

            div_ [class_ "form-group row"] $ do
              _ <- label_ [class_ "col-sm-3 form-control-label", for_ "password"] "Password:"
              div_ [class_ "col-sm-9"] $
                input_ [id_ "password", class_ "form-control", type_ "password"]

            div_ [class_ "form-group row"] $
              div_ [class_ "col-sm-offset-3 col-sm-9"] $ do
                button_ [class_ "btn btn-primary", id_ "login-btn"] "Log in"
                button_ [class_ "btn btn-secondary", id_ "signup-btn"] "Sign up"

        script_ [src_ "/js/login.js"] ("" :: String)
