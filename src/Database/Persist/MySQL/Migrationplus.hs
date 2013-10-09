{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.MySQL.Migrationplus
 ( getSqlCode
 , persistL
 , persistU
 , withMySQLPool'
 , withMySQLConn'
 , createMySQLPool'
 ) where

import Language.Haskell.TH.Quote (QuasiQuoter)

import qualified Database.MySQL.Simple as MySQL (ConnectInfo(..))
import Database.Persist.MySQL hiding (Statement, Update)
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

mysqlExtrasValidate :: ExtraCapabilities SqlUnit
mysqlExtrasValidate =  ExtraCapabilities
                        validateTriggers
                        -- ^ validate triggers definition and SQL using hssqlppp
                        doesNotSupport
                        -- ^ does not support create index atm

-- | Lower Case quasiquote with custom SQL
persistL :: [SqlUnit] -> QuasiQuoter
persistL extras =
  persistWith
    lowerCaseSettings
      { validateExtras = validateExtras' mysqlExtrasValidate extras }

-- | Uppper Case quasiquote with custom SQL
persistU :: [SqlUnit] -> QuasiQuoter
persistU extras =
  persistWith
    upperCaseSettings
      { validateExtras = validateExtras' mysqlExtrasValidate extras }

-- | Similar to @withMySQLPool , but with additional user defined
-- migration code generation tool, to add custom SQL code to the migration
-- process.
withMySQLPool'
  :: MonadIO m
  => CustomSql c -- ^ Custom SQL to be executed at migration
  -> MySQL.ConnectInfo -- ^ Connection information.
  -> Int -- ^ Number of connections to be kept open in the pool.
  -> (ConnectionPool -> m a)
  -- ^ Action to be executed that uses the connection pool.
  -> m a
withMySQLPool' csql ci = withSqlPool $ open' csql ci

-- | Similar to @createMySQLPool , but with additional user defined
-- migration code generation tool, to add custom SQL code to the migration
-- process.
createMySQLPool'
  :: MonadIO m
  => CustomSql c -- ^ Custom SQL to be executed at migration
  -> MySQL.ConnectInfo -- ^ Connection information.
  -> Int -- ^ Number of connections to be kept open in the pool.
  -> m ConnectionPool
createMySQLPool' csql ci = createSqlPool $ open' csql ci

-- | Similar to @withMySQLConn , but with additional user defined
-- migration code generation tool, to add custom SQL code to the migration
-- process.
withMySQLConn'
  :: (MonadBaseControl IO m, MonadIO m)
  => CustomSql c -- ^ Custom SQL to be executed at migration
  -> MySQL.ConnectInfo -- ^ Connection information.
  -> (Connection -> m a)
  -- ^ Action to be executed that uses the connection pool.
  -> m a
withMySQLConn' csql = withSqlConn . (open' csql)

open' :: CustomSql c -> MySQL.ConnectInfo -> IO Connection
open' = openSimpleConn'

-- | The two types of trigger supported by MySQL tables.
-- Views also support 'Instead Of', but views are not supported ATM.
data MySQLTriggerType = BEFORE | AFTER
  deriving (Eq,Read)

instance Show MySQLTriggerType where
  show BEFORE    = "BEFORE"
  show AFTER     = "AFTER"

-- | Does not support "UPDATE OF" syntax
data MySQLTriggerEvent =  INSERT | UPDATE | DELETE
  deriving (Eq,Show,Read)

-- | Determines if given trigger name exists in passed SQL
isSqlTrigger :: [SqlUnit] -> String -> Bool
isSqlTrigger sql name
  = maybe False (not . null . parseMySQL . snd)
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
               $ (reads (T.unpack triggerType) :: [(MySQLTriggerType,String)]))
             || error ("Invalid trigger type:" ++ show triggerType)
          t3 = ( not . null
               $ (reads (T.unpack triggerEvn) :: [(MySQLTriggerEvent,String)]))
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
getSqlCode :: GetCustomSql SqlUnit
getSqlCode sql tn (entry,line) =
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
                        (LT.unpack . LT.replace "\n" " " . snd . fromJust
                          $ find (\x -> fst x == T.unpack fn) sql)
              in if length values == 3
                    then ct
                    else error $ "Invalid Trigger Specification" ++ show values
            _ -> error "Only triggers supported for the moment"
          in T.pack result

          where readEvent e = let
                  event = reads e :: [(MySQLTriggerEvent,String)]
                  in  if not . null $ event
                       then fst . head $ event
                       else error $ "Invalid Trigger Event:" ++ show e
                readWhen w = let
                  when = reads w :: [(MySQLTriggerType,String)]
                  in  if not . null $ when
                        then head $ fmap fst when
                        else error $ "Invalid Trigger Type:" ++ show w

-- | Parse text using HsSqlPpp
-- It does not support MySQL syntax, but since it similar enough...
parseMySQL :: LT.Text -> [Statement]
parseMySQL sql = let
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
                    -> MySQLTriggerType
                    -> MySQLTriggerEvent
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
  ++ fn

