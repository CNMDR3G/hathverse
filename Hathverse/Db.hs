{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE EmptyDataDecls             #-}

module Hathverse.Db (
  runPqPool
, SqlPool
, allProblemIdTitles
) where

import Data.Text (Text)
import Data.Int (Int64)
import Control.Arrow
import Database.Persist.TH
import Database.Persist.Postgresql
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStderrLoggingT)
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

runPqPool :: (ConnectionPool -> IO ()) -> IO ()
runPqPool action =
  runStderrLoggingT . withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    runDb pool $ runMigration migrateAll
    action pool

type Query a = forall m. MonadIO m => SqlPersistT m a
type SqlPool = Pool SqlBackend

runDb :: SqlPool -> SqlPersistM a -> IO a
runDb = flip runSqlPersistMPool

allProblemIdTitles :: SqlPool -> IO [(Int64, Text)]
allProblemIdTitles pool = runDb pool $ do
  idTitles <- select $
    from $ \problem -> do
      orderBy [asc (problem ^. ProblemId)]
      return (problem ^.ProblemId, problem ^.ProblemTitle)
  return $ ((fromSqlKey . unValue) *** unValue) <$> idTitles
