{-# LANGUAGE OverloadedStrings #-}
module Db (
  Problem(..)
, withDb
) where

import Control.Exception
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

-- | Initilize PostgreSQL database:
-- > initdb --locale en_US.UTF-8 -E UTF8 -D '/usr/local/var/postgres'
-- > createuser -s -e -d hathverse
-- > createdb hathverse -U hathverse
--
-- in `psql`:
-- > create table problem (id serial primary key, title text not null, description text not null, template text not null, modulename text not null, solution text not null);

data Problem = Problem { pid :: Int
                       , title :: T.Text
                       , description :: T.Text
                       , template :: T.Text
                       , moduleName :: T.Text
                       , solution :: T.Text
                       }

instance FromRow Problem where
  fromRow = Problem <$> field <*> field <*> field
                    <*> field <*> field <*> field

dbConfig :: ConnectInfo
dbConfig = defaultConnectInfo {
             connectUser = "hathverse"
           , connectDatabase = "hathverse"
           }

withDb :: (Connection -> IO a) -> IO a
withDb = bracket (connect dbConfig) close
