{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE EmptyDataDecls             #-}

module Hathverse.Db (
  runConnPool
, Query
, Problem(..)
, User(..)
, allProblemIdTitles
, getProblemById
, addUser
, getUserByUsername
) where

import Data.Text (Text,pack)
import Data.Int (Int64)
import Control.Arrow
import Database.Persist.TH
import Database.Persist.Postgresql (ConnectionString, withPostgresqlPool)
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Database.Esqueleto
import Data.Pool (Pool)


-- | Initilize PostgreSQL database:
-- > initdb --locale en_US.UTF-8 -E UTF8 -D '/usr/local/var/postgres'
-- > createuser -s -e -d hathverse
-- > createdb hathverse -U hathverse

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Problem
    title        Text
    description  Text
    template     Text
    moduleName   Text
    checkProgram Text
    deriving     Show
User
    name        Text
    fullname    Text
    password    Text
    Primary     name
    deriving    Show
|]

connStr :: ConnectionString
connStr = "host=localhost dbname=hathverse user=hathverse"

runConnPool :: (ConnectionPool -> IO ()) -> IO ()
runConnPool action =
  runResourceT . runNoLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    runSqlPersistMPool (runMigration migrateAll) pool
    action pool

-- type ConnPool = Pool SqlBackend
type Query a = ReaderT SqlBackend IO a

runDb :: SqlPersistM a -> Query a
runDb query = ask >>= lift . runSqlPersistM query

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

getUserByUsername::Text -> Query (Maybe User)
getUserByUsername username = runDb $ do
    users <- select $
        from $ \user -> do
            where_ (user ^. UserName ==.  val username)
            limit 1
            return user
    case users of
        [user] -> return $ Just . entityVal $ user
        _ -> return Nothing

addUser::Text -> Text ->Text ->Query (Key User)
addUser username fullname hashPassword = runDb $ do
    uid <- insert $ User username fullname hashPassword
    return uid
