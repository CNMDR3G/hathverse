{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE EmptyDataDecls             #-}

module Hathverse.Db (
  runSql
, SqlPool
, Query
, Problem(..)
, allProblemIdTitles
, getProblemById
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
|]

connStr :: ConnectionString
connStr = "host=localhost dbname=hathverse user=hathverse"

runSql :: (ConnectionPool -> IO ()) -> IO ()
runSql action =
  runResourceT . runNoLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    runSqlPersistMPool (runMigration migrateAll) pool
    action pool

type SqlPool = Pool SqlBackend
type Query a = ReaderT SqlPool IO a

runDb :: SqlPersistM a -> Query a
runDb query = ask >>= lift . runSqlPersistMPool query

allProblemIdTitles :: Query [(Int64, Text)]
allProblemIdTitles = runDb $ do
  idTitles <- select $
    from $ \problem -> do
      orderBy [asc (problem ^. ProblemId)]
      return (problem ^. ProblemId, problem ^. ProblemTitle)
  return $ ((fromSqlKey . unValue) *** unValue) <$> idTitles

getProblemById :: Int64 -> Query (Maybe (Int64, Problem))
getProblemById problemId = runDb $ do
  problems <- select $
    from $ \problem -> do
      where_ (problem ^. ProblemId ==. valkey problemId)
      limit 1
      return problem
  case problems of
    [problem] -> return . Just $ (fromSqlKey . entityKey) &&& entityVal $ problem
    _ -> return Nothing
