{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.Reader
import System.Environment (lookupEnv)
import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Hathverse.Db (runSql)
import Hathverse.Css
import Hathverse.Controller

main :: IO ()
main = runSql $ \pool -> do

  port <- maybe 3000 read <$> lookupEnv "PORT"

  scotty port $ do

    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static"

    get "/css/default.css" $ do
      addHeader "Content-Type" "text/css"
      text defaultCss

    get "/" $ liftIO (runReaderT homepage pool) >>= html
