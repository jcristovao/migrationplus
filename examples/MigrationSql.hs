module MigrationSql
  ( persistLowerWithSql
  , persistUpperWithSql
  , module Triggers
  ) where

import Triggers
import Database.Persist.Postgresql.Migrationplus

sql = concat [triggers]

persistLowerWithSql = persistL sql

persistUpperWithSql = persistU sql
