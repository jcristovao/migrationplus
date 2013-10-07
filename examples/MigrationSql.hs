{-# LANGUAGE CPP #-}
module MigrationSql
  ( persistLowerWithSql
  , persistUpperWithSql
#if WITH_POSTGRESQL
  , module Database.Persist.Postgresql.Migrationplus
  , module Triggers
#elif WITH_SQLITE
  , module Database.Persist.Sqlite.Migrationplus
  , module SqliteTriggers
#endif
  ) where

#if WITH_POSTGRESQL
import Triggers
#elif WITH_SQLITE
import SqliteTriggers
#endif

#if WITH_POSTGRESQL
import Database.Persist.Postgresql.Migrationplus
#elif WITH_SQLITE
import Database.Persist.Sqlite.Migrationplus
#endif

sql = concat [triggers]

persistLowerWithSql = persistL sql

persistUpperWithSql = persistU sql
