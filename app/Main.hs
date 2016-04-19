{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main (main) where

import Data.Int (Int64)
import Control.Monad.Reader
import System.Environment (lookupEnv)
import Web.Spock.Safe
-- import Web.Spock.Lucid
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Hathverse.Db
import Hathverse.Controller

main :: IO ()
main = runConnPool $ \pool -> do

  let sessionCfg = Nothing  -- { sc_cookieName = "hathverse" }
      appCfg = defaultSpockCfg sessionCfg (PCPool pool) ()

  port <- maybe 3000 read <$> lookupEnv "PORT"
  runSpock port $ spock appCfg app


app :: SpockM SqlBackend (Maybe (Int64, User)) state ()
app = do

    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static"

    get root $ lazyBytes =<< runQuery' homepage

    get ("problems" <//> var) $ \pid ->
      lazyBytes =<< runQuery' (problemPage pid)

    get "edit" $ requireAuth $ \user ->
      lazyBytes =<< runQuery' (problemEditPage Nothing user)

    get ("edit" <//> var) $ \pid -> requireAuth $ \user ->
      lazyBytes =<< runQuery' (problemEditPage pid user)

    get "login" $ do
      sess <- readSession
      case sess of
        Just _ -> redirect "/"
        Nothing -> lazyBytes =<< runQuery' loginPage

    post "login" $ do
      username <- param' "username"
      password <- param' "password"
      _type <- param' "type"
      (j, sess) <- runQuery' $ loginSignupPost username password _type
      writeSession sess
      json j

    get "logout" $ do
      writeSession Nothing
      redirect "login"

    post "check" $
      json =<< runQuery' . checkApi =<< jsonBody'

  where
    requireAuth action = do
      sess <- readSession
      case sess of
        Nothing -> redirect "login"
        Just user -> action user

    runQuery' action = do
      sess <- readSession
      runQuery $ \conn ->
        liftIO (runReaderT action (Env conn sess))
