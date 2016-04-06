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
, allProblemIdTitles
) where

import Data.Text (Text)
import Data.Int (Int64)
import Control.Arrow
import Database.Persist.TH
import Database.Persist.Postgresql
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
    runDb pool $ runMigration migrateAll
    action pool

type SqlPool = Pool SqlBackend

runDb :: SqlPool -> SqlPersistM a -> IO a
runDb = flip runSqlPersistMPool

allProblemIdTitles :: ReaderT SqlPool IO [(Int64, Text)]
allProblemIdTitles = do
  pool <- ask
  lift . runDb pool $ do
  idTitles <- select $
    from $ \problem -> do
      orderBy [asc (problem ^. ProblemId)]
      return (problem ^.ProblemId, problem ^.ProblemTitle)
  return $ ((fromSqlKey . unValue) *** unValue) <$> idTitles
