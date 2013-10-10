{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Init (
    runConn'
  , sqlite_database
  , pgconn

   -- re-exports
  , module Database.Persist
  , module Test.Hspec
  , module Test.Hspec.HUnit
  , module Test.HUnit
  , mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase
  , Text
) where

-- re-exports
import Test.Hspec
import Test.Hspec.HUnit
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase)

-- testing
import Test.HUnit ((@?=),(@=?), Assertion, assertFailure, assertBool)

import Database.Persist
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as LT

import Control.Monad.Logger

#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#elif WITH_MYSQL
import Database.Persist.MySQL
#elif WITH_SQLITE
import Database.Persist.Sqlite
#endif

import Control.Monad.Trans.Control (MonadBaseControl)

import Control.Monad.IO.Class

#if WITH_POSTGRESQL
import Database.Persist.Postgresql.Migrationplus
import Database.Persist.Postgresql
#elif WITH_SQLITE
import Database.Persist.Sqlite
import Database.Persist.Sqlite.Migrationplus
#elif WITH_MYSQL
import Database.Persist.MySQL
import Database.Persist.MySQL.Migrationplus
#endif

sqlite_database :: Text
sqlite_database = "testdb.sqlite3"

pgconn :: BS.ByteString
pgconn = "host=localhost port=5432 user=test dbname=test password=test"

-- sqlite_database = ":memory:"
runConn':: (MonadIO m, MonadBaseControl IO m)
        => CustomSql c
        -> SqlPersistT (NoLoggingT m) t -> m ()
runConn' csql f = runNoLoggingT $ do
#  if WITH_POSTGRESQL
    _<- withPostgresqlPool' csql pgconn 2 $ runSqlPool f
#  elif WITH_MYSQL
    _ <- withMySQLPool' csql defaultConnectInfo
                        { connectHost     = "localhost"
                        , connectUser     = "test"
                        , connectPassword = "test"
                        , connectDatabase = "test"
                        } 1 $ runSqlPool f
#  elif WITH_SQLITE
    _<- withSqlitePool' csql sqlite_database 1 $ runSqlPool f
#  endif
    return ()

