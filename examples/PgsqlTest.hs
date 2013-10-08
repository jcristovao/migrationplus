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

import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Maybe

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

listen :: IO String
listen = do
  conn <- PQ.connectdb pgconn
  lr   <- PQ.exec conn "LISTEN lower_case_table;"
  maybe (do {print "Failure to LISTEN"; return "";})
        (const $ waitNotification conn)
        lr
  where
    waitNotification conn = do
      _ <- PQ.consumeInput conn
      noti <- PQ.notifies conn
      threadDelay 300
      maybe (waitNotification conn) (return . BC.unpack . PQ.notifyExtra) noti

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
      -- check the update trigger
      liftIO $ value `shouldBe` [[PersistText "cba"]]
    -- check that notification was received
    updatedId <- liftIO $ wait as
    liftIO $ updatedId `shouldBe` "1"


asIO :: IO a -> IO a
asIO = id


