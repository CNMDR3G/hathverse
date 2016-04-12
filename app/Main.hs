{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Text.Lazy (toStrict)
import Control.Monad.Reader
import System.Environment (lookupEnv)
import Web.Spock.Safe
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import qualified Hathverse.Db as Db
import Hathverse.Controller

main :: IO ()
main = Db.runSql $ \pool -> do

  port <- maybe 3000 read <$> lookupEnv "PORT"

  runSpock port $ spockT id $ do

    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static"

    get root $
       html . toStrict =<< liftIO (Db.runQuery pool homepage)

    get ("problems" <//> var) $ \pid ->
       html . toStrict =<< liftIO (Db.runQuery pool $ problemPage pid)

    post "/check" $ do
      j <- jsonBody'
      json =<< liftIO (Db.runQuery pool $ checkApi j)
