{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
module PgsqlTest (specs) where

import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Map as Map
import qualified Data.Text as T

import Database.Persist.Postgresql
import qualified Database.PostgreSQL.LibPQ as PQ

import Init
import MigrationSql

-- Test lower case names
share [mkPersist sqlSettings, mkMigrate "lowerCaseMigrate"] [persistLowerWithSql|
LowerCaseTable id=my_id
    fullName Text
    Triggers
        tableIdTrig AFTER INSERT
        tableChgIns AFTER INSERT
        tableTrig BEFORE DELETE
RefTable
    someVal Int sql=something_else
    lct LowerCaseTableId
    UniqueRefTable someVal
|]

listen :: IO ()
listen = do
  conn <- PQ.connectdb pgconn
  lr   <- PQ.exec conn "LISTEN lower_case_table;"
  maybe (print "Failure to LISTEN") (const $ waitNotification conn) lr
  where
    waitNotification conn = do
      noti <- PQ.notifies conn
      maybe (return ()) (\ n -> print $ "::: NOTIFICATION :::" ++ show n) noti
      threadDelay 100
      waitNotification conn

specs :: Spec
specs = describe "Migration Plus Tests" $ do
  it "Check extra blocks" $ do
      entityExtra (entityDef (Nothing :: Maybe LowerCaseTable)) @?=
          Map.fromList
              [ ("Triggers"
              , map T.words [ "tableIdTrig AFTER INSERT"
                            , "tableChgIns AFTER INSERT"
                            , "tableTrig BEFORE DELETE"])
              ]
  it "Create the tables and run additional migration" $ asIO $ do
    runConn' (getSqlCode,triggers) $ do
      runMigration lowerCaseMigrate
  it "Activates the insertion trigger" $ asIO $ do
    as <- liftIO $ async $ listen
    runConn' (getSqlCode,triggers) $ do
      C.runResourceT $
        rawExecute "INSERT INTO lower_case_table VALUES (1,'abc');" [] C.$$ CL.sinkNull
      value <- C.runResourceT $
        rawQuery "SELECT full_name from lower_case_table WHERE my_id=1" []
          C.$$ CL.consume
      liftIO $ value `shouldBe` [[PersistText "cba"]]
    liftIO $ wait as


asIO :: IO a -> IO a
asIO = id


