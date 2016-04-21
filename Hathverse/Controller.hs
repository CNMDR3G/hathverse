{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Maybe
import Data.Time

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

data EditRequest = EditRequest {
    edittype :: Text
  , editpid :: Int64
  , title :: Text
  , desc :: Text
  , modulename :: Text
  , template :: Text
  , solution :: Text
  , checkprog :: Text
  } deriving Generic

instance FromJSON EditRequest

editPost :: Int64 -> EditRequest -> Query CheckResult
editPost uid EditRequest{..} = do
  orig <- getProblemById editpid
  let prob = Problem { -- fake one
          problemTitle = title
        , problemAuthorId =
            case orig of
              Nothing -> toSqlKey uid
              Just Problem{..} -> problemAuthorId
        , problemDescription = desc
        , problemModuleName = modulename
        , problemTemplate = template
        , problemSolution = solution
        , problemCheckProgram = checkprog
        , problemIsApproved =
            case orig of
              Nothing -> False -- not approved when created
              Just Problem{..} -> problemIsApproved
        }
  case edittype of
       -- just test the solution
       "run" -> lift $ check prob $ T.unpack solution
       -- insert or update
       "submit" ->
         if editpid == -1 {- new -}
           then do
             newid <- insertProblem prob
             return CheckResult {ok=True, output="New problem #" ++ show newid ++ " created."}
           else do
             updateProblem editpid prob
             return CheckResult {ok=True, output="Problem updated."}
       _ -> return CheckResult {ok=False, output="Unknown action."}

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
  , solCode :: Text
  } deriving Generic

instance FromJSON CheckRequest

checkApi :: Maybe Int64 -> CheckRequest -> Query CheckResult
checkApi maybeUid CheckRequest{..} = do
  prob <- getProblemById probId
  case prob of
    Nothing ->
      return CheckResult {ok=False, output="Problem not found."}
    Just problem -> do
      mSubId <- if probId /= -1 && isJust maybeUid
                   then do
                     let Just uid = maybeUid
                     t <- liftIO getCurrentTime
                     Just <$> addSubmission (toSqlKey uid) (toSqlKey probId) solCode t
                   else pure Nothing
      (result@CheckResult {..}) <- lift $ check problem $ T.unpack solCode
      case mSubId of
          Nothing -> pure ()
          Just subId -> updateSubmission (fromSqlKey subId) (T.pack output) ok
      pure result
