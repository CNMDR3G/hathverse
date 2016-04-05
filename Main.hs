{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (lookupEnv)
import Control.Monad.IO.Class (liftIO)
import Web.Scotty as S
import Network.Wai.Middleware.Static
import Hathverse.Db (runPqPool)
import Hathverse.Css
import Hathverse.Controller

main :: IO ()
main = runPqPool $ \pool -> do

  port <- maybe 3000 read <$> lookupEnv "PORT"

  scotty port $ do

    middleware $ staticPolicy $ addBase "static"

    S.get "/css/default.css" $ do
      addHeader "Content-Type" "text/css"
      text defaultCss

    get "/" $ liftIO (homepage pool) >>= html
