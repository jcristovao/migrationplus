{-# LANGUAGE CPP #-}
module MigrationSql
  ( persistLowerWithSql
  , persistUpperWithSql
#if WITH_POSTGRESQL
  , module Database.Persist.Postgresql.Migrationplus
  , module PgsqlTriggers
#elif WITH_SQLITE
  , module Database.Persist.Sqlite.Migrationplus
  , module SqliteTriggers
#endif
  ) where

#if WITH_POSTGRESQL
import Database.Persist.Postgresql.Migrationplus
import PgsqlTriggers
#elif WITH_SQLITE
import Database.Persist.Sqlite.Migrationplus
import SqliteTriggers
#endif

sql = concat [triggers]

persistLowerWithSql = persistL sql

persistUpperWithSql = persistU sql
