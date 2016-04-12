{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main (main) where

import Control.Monad.Reader
import System.Environment (lookupEnv)
import Web.Spock.Safe
import Web.Spock.Lucid
import Data.Text.Lazy (toStrict)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import qualified Hathverse.Db as Db
import Hathverse.Controller
import Data.Text.Encoding (decodeUtf8,encodeUtf8)
import Crypto.PasswordStore



main :: IO ()
main = Db.runConnPool $ \pool -> do

  let sessionCfg =  Nothing  -- { sc_cookieName = "hathverse" }
      appCfg = defaultSpockCfg sessionCfg (PCPool pool) ()

  port <- maybe 3000 read <$> lookupEnv "PORT"
  runSpock port $ spock appCfg app


app :: SpockM _  (Maybe Db.User) state ()
app = do

    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static"


    get root $ requireUser $ \user ->
      lucid =<< runQuery' homepage

    get ("problems" <//> var) $ \pid -> requireUser $ \user ->
      lucid =<< runQuery' (problemPage pid)

    get "/login" $
      lucid  $ loginPage  "Please login"

    post "/login" $ do
        u <- param' "username"
        p <- param' "password"
        users <-  runQuery' $ Db.getUserByUsername u
        case users of
            Nothing -> lucid $ loginPage "The user don't exist"
            Just user -> case (verifyPassword (encodeUtf8 p) $ encodeUtf8 (Db.userPassword user)) of
                True -> do
                    writeSession  $ Just user
                    redirect "/"
                False -> lucid $  loginPage "The password is wrong "

    get "/signup" $ lucid $ signupPage

    post "/signup" $ do
        u <- param' "username"
        f <- param' "fullname"
        p <- param' "password"
        salt <- liftIO genSaltIO
        let password =  decodeUtf8 $ makePasswordSalt (encodeUtf8 p) salt 17
        userid <- runQuery' $ Db.addUser u f password
        lucid $ signupResultPage $ show userid


    post "/check" $ requireUser $ \user ->
       json =<< runQuery' . checkApi =<< jsonBody'

  where runQuery' action = runQuery $ \conn -> liftIO (runReaderT action conn)


requireUser action = do
    sess <- readSession
    case sess of
        Nothing -> redirect "/login"
        Just user -> action user
