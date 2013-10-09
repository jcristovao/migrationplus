{-# LANGUAGE CPP #-}
module MigrationSql
  ( persistLowerWithSql
  , persistUpperWithSql
#  if WITH_POSTGRESQL
  , module Database.Persist.Postgresql.Migrationplus
  , module PgsqlTriggers
#  elif WITH_SQLITE
  , module Database.Persist.Sqlite.Migrationplus
  , module SqliteTriggers
#  elif WITH_MYSQL
  , module Database.Persist.MySQL.Migrationplus
  , module MysqlTriggers
#  endif
  ) where

#if WITH_POSTGRESQL
import Database.Persist.Postgresql.Migrationplus
import PgsqlTriggers
#elif WITH_SQLITE
import Database.Persist.Sqlite.Migrationplus
import SqliteTriggers
#elif WITH_MYSQL
import Database.Persist.MySQL.Migrationplus
import MysqlTriggers
#endif

sql = concat [triggers]

persistLowerWithSql = persistL sql

persistUpperWithSql = persistU sql
