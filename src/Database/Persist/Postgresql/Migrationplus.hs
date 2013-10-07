{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Postgresql.Migrationplus
 ( getSqlCode
 , persistL
 , persistU
 , createPostgresqlPool'
 , withPostgresqlPool'
 , withPostgresqlConn'
 ) where

import Language.Haskell.TH.Quote (QuasiQuoter)

import Database.Persist.Postgresql hiding (Statement)
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

import Database.Persist.Migrationplus
import qualified Database.PostgreSQL.Simple as PG

psqlExtrasValidate :: ExtraCapabilities
psqlExtrasValidate = ExtraCapabilities
                        validateTriggers
                        -- ^ validate triggers definition and SQL using hssqlppp
                        doesNotSupport
                        -- ^ does not support create index atm

-- | Lower Case quasiquote with custom SQL
persistL :: [LT.Text] -> QuasiQuoter
persistL extras =
  persistWith
    lowerCaseSettings
      { validateExtras = (validateExtras' psqlExtrasValidate) extras }

-- | Uppper Case quasiquote with custom SQL
persistU :: [LT.Text] -> QuasiQuoter
persistU extras =
  persistWith
    upperCaseSettings
      { validateExtras = validateExtras' psqlExtrasValidate extras }

-- | open a PostgresSQL connection with the provided
-- custom migration function
open' :: ExtrasSql e -> ConnectionString -> IO Connection
open' gsql cstr = do
    PG.connectPostgreSQL cstr >>= openSimpleConn' gsql


-- | The two types of trigger supported by PostgreSQL tables.
-- Views also support 'Instead Of', but views are not supported ATM.
data PostgreSqlTriggerType = BEFORE | AFTER
  deriving (Eq,Read)

instance Show PostgreSqlTriggerType where
  show BEFORE    = "BEFORE"
  show AFTER     = "AFTER"

-- | Does not support "UPDATE OF" syntax
data PostgreSqlTriggerEvent =  INSERT | UPDATE | DELETE | TRUNCATE | OR
  deriving (Eq,Show,Read)

-- | Determines if given trigger name exists in passed SQL
isSqlTrigger :: [LT.Text] -> String -> Bool
isSqlTrigger sql name = any (== Just (map toLower name, map toLower "trigger"))
                      $ fmap getSqlFuncAttrs
                      $ concat
                      $ fmap parsePostgreSQL sql

-- | Validate trigger entry in extras
validateTriggers :: [LT.Text]
                -> [[Text]]
                -> Bool
validateTriggers sql ps = all id $ map (validateTrigger sql) ps
  where validateTrigger sql' params = let
          triggerFunc = params !! 0
          triggerType = params !! 1
          triggerEvns = drop 2 params
          t1 = length params >= 3 || error ("Insufficient parameters" ++ show params)
          t2 = ( not . null
               $ (reads (T.unpack triggerType) :: [(PostgreSqlTriggerType,String)]))
             || error ("Invalid trigger type:" ++ show triggerType)
          t3 = ( all (\te ->  not . null
                     $ (reads (T.unpack te) :: [(PostgreSqlTriggerEvent,String)]))
                     $ triggerEvns)
             || error ("Invalid trigger events:" ++ show triggerEvns)
          t4 = (isSqlTrigger sql' $ T.unpack triggerFunc)
             || error ("There is no (Postgre)SQL function with name:" ++ show triggerFunc)
          in all id [t1,t2,t3,t4]

-- | Given a SQL statement, determines if it is a function, returning its name
-- and SQL.
getSqlFuncCode :: Statement -> Maybe (String,Statement)
getSqlFuncCode sql = case sql of
   (CreateFunction _ (Name _ ns) _ _ _ _ (PlpgsqlFnBody _ _) _)
      -> if null ns
          then Nothing
          else Just (ncStr $ last ns, sql)
   _  -> Nothing


-- | Get SQL Function name and type
getSqlFuncAttrs :: Statement -> Maybe (String,String)
getSqlFuncAttrs sql = case sql of
   (CreateFunction _ (Name _ fns) _ (SimpleTypeName _ (Name _ tns)) _ _ _ _)
      -> if null fns || null tns
            then Nothing
            else Just (ncStr $ last fns,ncStr $ last tns)
   _  -> Nothing


-- | Get SQL code for extras.
-- Currently only supports Triggers
getSqlCode :: GetExtrasSql LT.Text
getSqlCode triggers tn (entry,line) =
    T.concat $ map getSqlCode' line
  where getSqlCode' values = let
          result = case entry of
            "Triggers" -> let
              fn    = values !! 0
              when  = T.unpack $ values !! 1
              tevns = map (T.unpack) $ drop 2 values
              -- trigger function
              tf  = maybe ""
                    ( LT.unpack
                    . printStatements (PrettyPrintFlags PostgreSQLDialect)
                    . replicate 1
                    . snd)
                  $ find (\(n,_) -> n == (T.unpack . T.toLower) fn)
                  $ catMaybes
                  $ map (getSqlFuncCode . head . parsePostgreSQL) triggers
              -- trigger events
              ct   = createRowTrigger'
                        (T.unpack fn)
                        (readWhen when)
                        (concat $ map readEvent tevns)
                        (T.unpack tn)
                        (T.unpack fn)
              in if length values >= 3
                    then tf ++ ct
                    else error $ "Invalid Trigger Specification" ++ show values
            _ -> error "Only PostgreSQL triggers supported for the moment"
          in T.pack result

          where readEvent e = let
                  event = reads e :: [(PostgreSqlTriggerEvent,String)]
                  in  if not . null $ event
                       then fmap fst event
                       else error $ "Invalid Trigger Event:" ++ show e
                readWhen w = let
                  when = reads w :: [(PostgreSqlTriggerType,String)]
                  in  if not . null $ when
                        then head $ fmap fst when
                        else error $ "Invalid Trigger Type:" ++ show w

-- | Parse text using HsSqlPpp
parsePostgreSQL :: LT.Text -> [Statement]
parsePostgreSQL sql = let
  res = parsePlpgsql
    (ParseFlags PostgreSQLDialect)
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
createRowTrigger' :: String
                    -> PostgreSqlTriggerType
                    -> [PostgreSqlTriggerEvent]
                    -> String
                    -> String
                    -> String
createRowTrigger' name when events table fn =
    LT.unpack
  . printStatements (PrettyPrintFlags PostgreSQLDialect)
  $ createRowTrigger name when events table fn


-- | Convert Internal representation to HsSqlPpp
convertTriggerEvents :: [PostgreSqlTriggerEvent] -> [TriggerEvent]
convertTriggerEvents tes = let
  conv e = case e of
    UPDATE -> Just TUpdate
    INSERT -> Just TInsert
    DELETE -> Just TDelete
    TRUNCATE->Just TDelete
    OR     -> Nothing
  in mapMaybe conv tes

-- | Convert Internal representation to HsSqlPpp
convertTriggerType :: PostgreSqlTriggerType -> TriggerWhen
convertTriggerType tt = case tt of
  BEFORE    -> TriggerBefore
  AFTER     -> TriggerAfter

-- | Convert a trigger on row quasiquote into a valid statement
createRowTrigger
  :: String
  -> PostgreSqlTriggerType
  -> [PostgreSqlTriggerEvent]
  -> String
  -> String
  -> [Statement]
createRowTrigger name' typ' events' table' fn' = let
  annot  = Annotation (Just (__FILE__,__LINE__,0)) Nothing  []  Nothing  []
  name   = Nmc name'
  typ    = convertTriggerType typ'
  events = convertTriggerEvents events'
  table  = Name annot [Nmc table']
  fn     = Name annot [Nmc fn']
  createt= CreateTrigger annot name typ events table EachRow fn []
  dropt  = DropTrigger annot IfExists name table Cascade
  in [dropt,createt]

-------------------------------------------------------------------------------
-- Extra Stuff ----------------------------------------------------------------
-------------------------------------------------------------------------------

-- | Similar to @withPostgresqlConn , but with additional user defined
-- migration code generation tool, to add custom SQL code to the migration
-- process.
withPostgresqlPool' :: MonadIO m
                   => ExtrasSql e
                   -- ^ Function to get custom SQL to be executed
                   -- at migration
                   -> ConnectionString
                   -- ^ Connection string to the database.
                   -> Int
                   -- ^ Number of connections to be kept open in
                   -- the pool.
                   -> (ConnectionPool -> m a)
                   -- ^ Action to be executed that uses the
                   -- connection pool.
                   -> m a
withPostgresqlPool' gsql ci = withSqlPool $ open' gsql ci


-- | Similar to @createPostgresqlPool , but with additional user defined
-- migration code generation tool, to add custom SQL code to the migration
-- process.
createPostgresqlPool':: MonadIO m
                     => ExtrasSql e
                     -- ^ Function to get custom SQL to be executed
                     -- at migration
                     -> ConnectionString
                     -- ^ Connection string to the database.
                     -> Int
                     -- ^ Number of connections to be kept open
                     -- in the pool.
                     -> m ConnectionPool
createPostgresqlPool' gsql ci = createSqlPool $ open' gsql ci


-- | Similar to @withPostgresqlConn , but with additional user defined
-- migration code generation tool, to add custom SQL code to the migration
-- process.
withPostgresqlConn' :: (MonadIO m, MonadBaseControl IO m)
                   => ExtrasSql e
                   -> ConnectionString
                   -> (Connection -> m a)
                   -> m a
withPostgresqlConn' gsql = withSqlConn . (open' gsql)


