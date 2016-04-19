{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Hathverse.Controller where

import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.ByteString.Lazy (ByteString)
import Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Aeson
import Control.Monad.Reader
import Lucid
import Crypto.PasswordStore
import Hathverse.Db
import Hathverse.View
import Hathverse.View.Common
import Hathverse.Checker

runHtml :: HtmlGen -> Query ByteString
runHtml action = do
  maybeUser <- asks currUser
  return . flip runReader maybeUser $ renderBST action

homepage :: Query ByteString
homepage = do
  pidTitles <- allProblemIdTitles
  runHtml $ homepageView pidTitles

problemPage :: Int64 -> Query ByteString
problemPage pid = do
  prob <- getProblemById pid
  runHtml $ problemView pid prob

problemEditPage :: Maybe Int64 -> (Int64, User) -> Query ByteString
problemEditPage maybePid (uid, user) =
  case maybePid of
    Nothing -> runHtml $ problemEditView Nothing
    Just pid -> do
      maybeProb <- getProblemById pid
      case maybeProb of
        Nothing -> runHtml $ errorView "Problem not found."
        Just prob ->
          let authorId = fromSqlKey $ problemAuthorId prob in
          if userIsAdmin user || uid == authorId
            then runHtml $ problemEditView (Just (pid, prob))
            else runHtml $ errorView "Not authorized."

loginPage :: Query ByteString
loginPage = runHtml loginView

loginSignupPost :: Text -> Text -> Text -> Query (Value, Maybe (Int64, User))
loginSignupPost username password _type = do
  maybeUser <- getUserByUsername username
  (ok', err', sess) <-
    if T.length username < 6 || T.length password < 6
       then return (False, "Length of username and password should be at least 6.", Nothing)
       else
         case _type of
           "login" ->
             case maybeUser of
               Nothing -> return (False, "User not found.", Nothing)
               Just userWithId@(_, user) ->
                 if verifyPassword (encodeUtf8 password) $ encodeUtf8 (userPassword user)
                   then return (True, "success", Just userWithId)
                   else return (False, "Wrong password.", Nothing)
           "signup" ->
             case maybeUser of
               Just _ -> return (False, "Username is already used.", Nothing)
               Nothing -> do
                 salt <- liftIO genSaltIO
                 let hashPassword =  decodeUtf8 $ makePasswordSalt (encodeUtf8 password) salt 17
                 userid <- addUser username hashPassword
                 insertedUser <- getUserByUsername username
                 return (True, show userid, insertedUser)
           _ -> return (False, "?", Nothing)
  return (object ["ok" .= ok', "err" .= err'], sess)


data CheckRequest = CheckRequest {
    probId :: Int64
  , solCode :: String
  } deriving Generic

instance FromJSON CheckRequest

checkApi :: CheckRequest -> Query CheckResult
checkApi (CheckRequest pid code) = do
  prob <- getProblemById pid
  case prob of
    Nothing ->
      return CheckResult { ok=False , output = "Problem not found." }
    Just problem ->
      lift $ check problem code
