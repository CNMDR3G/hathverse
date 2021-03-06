{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main (main) where

import Data.Aeson (object, (.=))
import Data.Int (Int64)
import Control.Monad.Reader
import System.Environment (lookupEnv)
import Web.Spock.Safe
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Hathverse.Db
import Hathverse.Controller

main :: IO ()
main = runConnPool $ \pool -> do

  let sessionCfg = (defaultSessionCfg Nothing) {
                     sc_cookieName = "hathverse"
                   , sc_sessionTTL = 604800 -- one week
                   }
      appCfg = SpockCfg {
                 spc_initialState = ()
               , spc_database = PCPool pool
               , spc_sessionCfg = sessionCfg
               , spc_maxRequestSize = Just (5 * 1024 * 1024)
               }

  port <- maybe 3000 read <$> lookupEnv "PORT"
  runSpock port $ spock appCfg app


app :: SpockM SqlBackend (Maybe (Int64, User)) state ()
app = do

    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static"

    get root $ lazyBytes =<< runQuery' homepage

    get ("problems" <//> var) $ \pid ->
      lazyBytes =<< runQuery' (problemPage pid)

    post "problems/test" $
      json =<< runQuery' . testPost =<< jsonBody'

    get "problems/new" $
      requireAuth (redirect "/login") $ \user ->
        lazyBytes =<< runQuery' (problemEditPage Nothing user)

    post "problems/new" $
      let onfail = json $ object ["ok" .= False, "err" .= ("Session expired." :: String)] in
      requireAuth onfail $ \(uid, _) ->
        json =<< runQuery' . newPost uid =<< jsonBody'

    get ("problems" <//> var <//> "edit") $ \pid ->
      requireAuth (redirect "/login") $ \user ->
        lazyBytes =<< runQuery' (problemEditPage (Just pid) user)

    post ("problems" <//> var <//> "edit") $ \pid ->
      let onfail = json $ object ["ok" .= False, "err" .= ("Session expired." :: String)] in
      requireAuth onfail $ \_ ->
          json =<< runQuery' . editPost pid =<< jsonBody'

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
      redirect "/login"

    post "check" $ do
      sess <- readSession
      json =<< runQuery' . checkApi (fst <$> sess) =<< jsonBody'

  where
    requireAuth onfail action = do
      sess <- readSession
      case sess of
        Nothing -> onfail
        Just user -> action user

    runQuery' action = do
      sess <- readSession
      runQuery $ \conn ->
        liftIO (runReaderT action (Env conn sess))
