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

import Database.Persist.Postgresql.Migrationplus
import Database.Persist.Postgresql

sqlite_database :: Text
sqlite_database = "test/testdb.sqlite3"

-- sqlite_database = ":memory:"
runConn':: (MonadIO m, MonadBaseControl IO m)
        => ExtrasSql LT.Text
        -> SqlPersistT (NoLoggingT m) t -> m ()
runConn' esql f = runNoLoggingT $ do
#  if WITH_POSTGRESQL
    _<- withPostgresqlPool' esql "host=localhost port=5432 user=test dbname=test password=test" 1 $ runSqlPool f
#  elif WITH_MYSQL
    _ <- withMySQLPool defaultConnectInfo
                        { connectHost     = "localhost"
                        , connectUser     = "test"
                        , connectPassword = "test"
                        , connectDatabase = "test"
                        } 1 $ runSqlPool f
#  elif WITH_SQLITE
    _<-withSqlitePool sqlite_database 1 $ runSqlPool f
#  endif
    return ()

