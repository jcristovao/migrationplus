{-# LANGUAGE CPP #-}
module MigrationSql
  ( persistLowerWithSql
  , persistUpperWithSql
#  if WITH_POSTGRESQL
  , module Database.Persist.Postgresql.Migrationplus
  , module PgsqlSql
#  elif WITH_SQLITE
  , module Database.Persist.Sqlite.Migrationplus
  , module SqliteSql
#  elif WITH_MYSQL
  , module Database.Persist.MySQL.Migrationplus
  , module MysqlSql
#  endif
  ) where

#if WITH_POSTGRESQL
import Database.Persist.Postgresql.Migrationplus
import PgsqlSql
#elif WITH_SQLITE
import Database.Persist.Sqlite.Migrationplus
import SqliteSql
#elif WITH_MYSQL
import Database.Persist.MySQL.Migrationplus
import MysqlSql
#endif

persistLowerWithSql = persistL sql

persistUpperWithSql = persistU sql
