{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Hathverse.Db (
  runConnPool
, Query
, Problem(..)
, User(..)
, Submission(..)
, Env(..)
, allProblemIdTitles
, getProblemById
, updateProblem
, insertProblem
, addUser
, getUserByUsername
, SqlBackend
, fromSqlKey
, toSqlKey
, addSubmission
, updateSubmission
) where

import Data.Text (Text)
import Data.Int (Int64)
import Control.Arrow
import Database.Persist.TH
import Database.Persist.Postgresql (ConnectionString, withPostgresqlPool)
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Database.Esqueleto
import Data.Time
import GHC.Generics (Generic)
import Data.Aeson

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Problem
    title        Text
    authorId     UserId
    description  Text
    moduleName   Text
    template     Text
    solution     Text
    checkProgram Text
    isApproved   Bool
    deriving     Generic
User
    name         Text
    UniqueName   name
    password     Text
    isAdmin      Bool
    deriving     Show
Submission
    userId       UserId
    problemId    ProblemId
    sourceCode   Text
    result       Text Maybe
    accepted     Bool Maybe
    date         UTCTime
    deriving     Show
|]

instance FromJSON Problem

connStr :: ConnectionString
connStr = "host=localhost dbname=hathverse user=hathverse"

runConnPool :: (ConnectionPool -> IO ()) -> IO ()
runConnPool action =
  runResourceT . runNoLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    runSqlPersistMPool (runMigration migrateAll) pool
    action pool

data Env = Env {
    sqlHandler :: SqlBackend
  , currUser :: Maybe (Int64, User)
  }

type Query a = ReaderT Env IO a

runDb :: SqlPersistM a -> Query a
runDb query = asks sqlHandler >>= lift . runSqlPersistM query

allProblemIdTitles :: Query [(Int64, Text)]
allProblemIdTitles = runDb $ do
  idTitles <- select $
    from $ \problem -> do
      orderBy [asc (problem ^. ProblemId)]
      return (problem ^. ProblemId, problem ^. ProblemTitle)
  return $ ((fromSqlKey . unValue) *** unValue) <$> idTitles

getProblemById :: Int64 -> Query (Maybe Problem)
getProblemById problemId = runDb $ do
  problems <- select $
    from $ \problem -> do
      where_ (problem ^. ProblemId ==. valkey problemId)
      limit 1
      return problem
  case problems of
    [problem] -> return . Just . entityVal $ problem
    _ -> return Nothing

updateProblem :: Int64 -> Problem -> Query ()
updateProblem pid Problem{..} = runDb $
  update $ \prob -> do
    set prob [
            ProblemTitle        =. val problemTitle
          , ProblemDescription  =. val problemDescription
          , ProblemModuleName   =. val problemModuleName
          , ProblemTemplate     =. val problemTemplate
          , ProblemSolution     =. val problemSolution
          , ProblemCheckProgram =. val problemCheckProgram
          ]
    where_ (prob ^. ProblemId ==. valkey pid)

insertProblem :: Problem -> Query Int64
insertProblem prob = fromSqlKey <$> runDb (insert prob)

getUserByUsername :: Text -> Query (Maybe (Int64, User))
getUserByUsername username = runDb $ do
  users <- select $
    from $ \user -> do
      where_ (user ^. UserName ==. val username)
      limit 1
      return user
  case users of
    [user] -> return . Just $
      ((fromSqlKey . entityKey) &&& entityVal) user
    _ -> return Nothing

addUser :: Text -> Text -> Query (Key User)
addUser username hashPassword =
  runDb . insert $ User username hashPassword True -- all are admin for testing now

addSubmission :: Key User -> Key Problem -> Text -> UTCTime -> Query (Key Submission)
addSubmission uid pid srcContent dt =
    runDb . insert $ Submission uid pid srcContent Nothing Nothing dt

updateSubmission :: Int64 -> Text -> Bool -> Query ()
updateSubmission subId result accepted = runDb $
    update $ \submission -> do
        set submission
            [ SubmissionResult =. val (Just result)
            , SubmissionAccepted =. val (Just accepted)
            ]
        where_ (submission ^. SubmissionId ==. valkey subId)
