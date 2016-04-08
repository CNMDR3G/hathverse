{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.Reader
import System.Environment (lookupEnv)
import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Hathverse.Db (runSql, runQuery)
import Hathverse.Controller

main :: IO ()
main = runSql $ \pool -> do

  port <- maybe 3000 read <$> lookupEnv "PORT"

  scotty port $ do

    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static"

    get "/" $ liftIO (runQuery pool homepage) >>= html

    get "/problems/:pid" $ do
      pid <- read <$> param "pid"
      liftIO (runQuery pool $ problemPage pid) >>= html

    post "/check" $ do
      j <- jsonData
      liftIO (runQuery pool $ checkApi j) >>= json
