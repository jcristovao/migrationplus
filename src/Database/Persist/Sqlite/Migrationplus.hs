{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Sqlite.Migrationplus
 ( getSqlCode
 , persistL
 , persistU
 , withSqlitePool'
 , withSqliteConn'
 , createSqlitePool'
 ) where

import Language.Haskell.TH.Quote (QuasiQuoter)

import qualified Database.Sqlite as Sqlite (open)
import Database.Persist.Sqlite (wrapConnection',ExtrasSql,GetExtrasSql,Connection,withSqlPool,ConnectionPool,createSqlPool,withSqlPool,withSqlConn)
import Database.Persist.TH (persistWith)
import Database.Persist.Quasi

import Database.HsSqlPpp.Ast
import Database.HsSqlPpp.Pretty
import Database.HsSqlPpp.Annotation
import Database.HsSqlPpp.Parser

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text (Text)

import Data.Maybe
import Data.List
import Data.Char

import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad ((>=>))

import Database.Persist.Migrationplus

import Debug.Trace

sqliteExtrasValidate :: ExtraCapabilities SqlUnit
sqliteExtrasValidate =  ExtraCapabilities
                        validateTriggers
                        -- ^ validate triggers definition and SQL using hssqlppp
                        doesNotSupport
                        -- ^ does not support create index atm

-- | Lower Case quasiquote with custom SQL
persistL :: [SqlUnit] -> QuasiQuoter
persistL extras =
  persistWith
    lowerCaseSettings
      { validateExtras = validateExtras' sqliteExtrasValidate extras }

-- | Uppper Case quasiquote with custom SQL
persistU :: [SqlUnit] -> QuasiQuoter
persistU extras =
  persistWith
    upperCaseSettings
      { validateExtras = validateExtras' sqliteExtrasValidate extras }

createSqlitePool'
  :: MonadIO m
  => ExtrasSql e -- ^ Function to get custom SQL to be executed at migration
  -> Text
  -> Int
  -> m ConnectionPool
createSqlitePool' gsql s = createSqlPool $ open' gsql s

withSqlitePool'
  :: (MonadBaseControl IO m, MonadIO m)
  => ExtrasSql e -- ^ Function to get custom SQL to be executed
  -> Text
  -> Int -- ^ number of connections to open
  -> (ConnectionPool -> m a) -> m a
withSqlitePool' gsql s = withSqlPool $ open' gsql s

withSqliteConn'
  :: (MonadBaseControl IO m, MonadIO m)
  => ExtrasSql e -- ^ Function to get custom SQL to be executed
  -> Text
  -> (Connection -> m a)
  -> m a
withSqliteConn' gsql = withSqlConn . open' gsql

open' :: ExtrasSql e -> Text -> IO Connection
open' gsql = Sqlite.open >=> wrapConnection' gsql


-- | The two types of trigger supported by Sqlite tables.
-- Views also support 'Instead Of', but views are not supported ATM.
data PostgreSqlTriggerType = BEFORE | AFTER
  deriving (Eq,Read)

instance Show PostgreSqlTriggerType where
  show BEFORE    = "BEFORE"
  show AFTER     = "AFTER"

-- | Does not support "UPDATE OF" syntax
data SqliteTriggerEvent =  INSERT | UPDATE | DELETE | TRUNCATE | OR
  deriving (Eq,Show,Read)

-- | Determines if given trigger name exists in passed SQL
isSqlTrigger :: [SqlUnit] -> String -> Bool
isSqlTrigger sql name
  = maybe False (not . null . parseSqlite . snd)
  $ find (\x -> name == fst x) sql

-- | Validate trigger entry in extras
validateTriggers :: [SqlUnit]
                -> [[Text]]
                -> Bool
validateTriggers sql ps = all id $ map (validateTrigger sql) ps
  where validateTrigger sql' params = let
          triggerFunc = params !! 0
          triggerType = params !! 1
          triggerEvn  = params !! 2
          t1 = length params == 3 || error ("Wrong number of parameters" ++ show params)
          t2 = ( not . null
               $ (reads (T.unpack triggerType) :: [(PostgreSqlTriggerType,String)]))
             || error ("Invalid trigger type:" ++ show triggerType)
          t3 = ( not . null
               $ (reads (T.unpack triggerEvn) :: [(SqliteTriggerEvent,String)]))
             || error ("Invalid trigger event:" ++ show triggerEvn)
          t4 = (isSqlTrigger sql' $ T.unpack triggerFunc)
             || error ("There is no SQL statement with name:" ++ show triggerFunc)
          in all id [t1,t2,t3,t4]

getSqlTrigCode :: Statement -> Maybe (String,Statement)
getSqlTrigCode sql = case sql of
  (Update _ (Name _ ns) _ _ _ _) -> nameAndSql ns
  (Insert _ (Name _ ns) _ _ _)   -> nameAndSql ns
  (Delete _ (Name _ ns) _ _ _)   -> nameAndSql ns
  _                              -> Nothing
  where
    nameAndSql ns = fmap (\n -> (ncStr n,sql)) $ listToMaybe ns


-- | Get SQL code for extras.
-- Currently only supports Triggers
getSqlCode :: GetExtrasSql SqlUnit
getSqlCode triggers tn (entry,line) =
    T.concat $ map getSqlCode' line
  where getSqlCode' values = let
          result = case entry of
            "Triggers" -> let
              fn   = values !! 0
              when = T.unpack $ values !! 1
              tevn = T.unpack $ values !! 2
              -- trigger events
              ct   = createRowTrigger
                        (T.unpack fn)
                        (readWhen when)
                        (readEvent tevn)
                        (T.unpack tn)
                        (LT.unpack . snd . fromJust
                          $ find (\x -> fst x == T.unpack fn) triggers)
              in if length values == 3
                    then ct
                    else error $ "Invalid Trigger Specification" ++ show values
            _ -> error "Only triggers supported for the moment"
          in T.pack result

          where readEvent e = let
                  event = reads e :: [(SqliteTriggerEvent,String)]
                  in  if not . null $ event
                       then fst . head $ event
                       else error $ "Invalid Trigger Event:" ++ show e
                readWhen w = let
                  when = reads w :: [(PostgreSqlTriggerType,String)]
                  in  if not . null $ when
                        then head $ fmap fst when
                        else error $ "Invalid Trigger Type:" ++ show w

-- | Parse text using HsSqlPpp
-- It does not support Sqlite syntax, but since it similar enough...
parseSqlite :: LT.Text -> [Statement]
parseSqlite sql = let
  res = parsePlpgsql
    (ParseFlags PostgreSQLDialect) -- TODO: Fix this
    __FILE__
    Nothing
    sql
  in either
      (\pe -> error $ show pe)
      (\s  -> if null s
                then error $ "Valid SQL returned no statements"
                else s)
      res

------------------------------------------------------------------------------
-- Create Triggers -----------------------------------------------------------
------------------------------------------------------------------------------
-- | Convert a trigger on a row into valid (usable) SQL
createRowTrigger  :: String
                    -> PostgreSqlTriggerType
                    -> SqliteTriggerEvent
                    -> String
                    -> String
                    -> String
createRowTrigger  name when event table fn =
      "CREATE TRIGGER "
  ++ name ++ " "
  ++ show when ++ " "
  ++ show event ++ " "
  ++ " ON "
  ++ table ++ " "
  ++ " FOR EACH ROW "
  ++ " BEGIN "
  ++ fn
  ++ " END;"


