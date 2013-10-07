{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Init (
    runConn
  , runConn'
  , db
  , sqlite_database

   -- re-exports
  , module Database.Persist
  , module Test.Hspec
  , module Test.Hspec.HUnit
  , module Test.HUnit
  , liftIO
  , mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase
  , Int32, Int64
  , Text
) where

-- re-exports
import Test.Hspec
import Test.Hspec.HUnit
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase)

-- testing
import Test.HUnit ((@?=),(@=?), Assertion, assertFailure, assertBool)
import Test.QuickCheck

import Database.Persist
import Data.Text (Text)
import qualified Data.Text.Lazy as LT

import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Logger

#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#elif WITH_MYSQL
import Database.Persist.MySQL
#else
import Database.Persist.Sqlite
#endif

import Control.Monad (unless)
import Control.Monad.Trans.Control (MonadBaseControl)

-- Data types
import Data.Int (Int32, Int64)

import Control.Monad.IO.Class
import System.Random

import Database.Persist.Postgresql
import Database.Persist.Postgresql.Migrationplus

sqlite_database :: Text
sqlite_database = "test/testdb.sqlite3"
-- sqlite_database = ":memory:"
runConn :: (MonadIO m, MonadBaseControl IO m)
        => SqlPersistT (NoLoggingT m) t -> m ()
runConn f = runNoLoggingT $ do
#  if defined(WITH_POSTGRESQL)
    _<-withPostgresqlPool "host=localhost port=5432 user=test dbname=test password=test" 1 $ runSqlPool f
#  elif defined(WITH_MYSQL)
    _ <- withMySQLPool defaultConnectInfo
                        { connectHost     = "localhost"
                        , connectUser     = "test"
                        , connectPassword = "test"
                        , connectDatabase = "test"
                        } 1 $ runSqlPool f
#  else
    _<-withSqlitePool sqlite_database 1 $ runSqlPool f
#  endif
    return ()

-- sqlite_database = ":memory:"
runConn':: (MonadIO m, MonadBaseControl IO m)
        => ExtrasSql LT.Text
        -> SqlPersistT (NoLoggingT m) t -> m ()
runConn' esql f = runNoLoggingT $ do
#  if defined(WITH_POSTGRESQL)
    _<-withPostgresqlPool' esql "host=localhost port=5432 user=test dbname=test password=test" 1 $ runSqlPool f
#  elif defined(WITH_MYSQL)
    _ <- withMySQLPool defaultConnectInfo
                        { connectHost     = "localhost"
                        , connectUser     = "test"
                        , connectPassword = "test"
                        , connectDatabase = "test"
                        } 1 $ runSqlPool f
#  else
    _<-withSqlitePool sqlite_database 1 $ runSqlPool f
#  endif
    return ()


db :: SqlPersistT (NoLoggingT (ResourceT IO)) () -> Assertion
db actions = do
  runResourceT $ runConn $ actions >> transactionUndo

