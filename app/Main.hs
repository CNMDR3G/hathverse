{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
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
main = Db.runConnPool $ \pool -> do

  let sessionCfg = (defaultSessionCfg ()) { sc_cookieName = "hathverse" }
      appCfg = defaultSpockCfg sessionCfg (PCPool pool) ()

  port <- maybe 3000 read <$> lookupEnv "PORT"
  runSpock port $ spock appCfg app


app :: SpockM _ ses state ()
app = do

    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static"

    get root $ do
      htmlText <- runQuery' homepage
      html . toStrict $ htmlText

    get ("problems" <//> var) $ \pid -> do
      htmlText <- runQuery' $ problemPage pid
      html . toStrict $ htmlText

    post "/check" $ do
      j <- jsonBody'
      reply <- runQuery' $ checkApi j
      json reply

  where runQuery' action = runQuery $ \conn -> liftIO (runReaderT action conn)
