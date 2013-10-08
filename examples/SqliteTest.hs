{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
module SqliteTest (specs) where

import Control.Monad.IO.Class
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Map as Map
import qualified Data.Text as T

import Database.Persist.Sqlite

import Init
import MigrationSql

-- Test lower case names
share [mkPersist sqlSettings, mkMigrate "lowerCaseMigrate"] [persistLowerWithSql|
LowerCaseTable id=my_id
    fullName Text
    Triggers
        tableIdTrig AFTER INSERT
        tableTrig BEFORE DELETE
RefTable
    someVal Int sql=something_else
    lct LowerCaseTableId
    UniqueRefTable someVal
|]

specs :: Spec
specs = describe "rename specs" $ do
  it "Check extra blocks" $ do
      entityExtra (entityDef (Nothing :: Maybe LowerCaseTable)) @?=
          Map.fromList
              [ ("Triggers"
              , map T.words ["tableIdTrig AFTER INSERT","tableTrig BEFORE DELETE"])
              ]
  it "Create the tables and run additional migration" $ asIO $ do
    runConn' (getSqlCode,triggers) $ do
      runMigration lowerCaseMigrate
  it "Activates the insertion trigger" $ asIO $ do
    runConn' (getSqlCode,triggers) $ do
      C.runResourceT $
        rawExecute "INSERT INTO lower_case_table VALUES (1,'abc');" [] C.$$ CL.sinkNull
      value <- C.runResourceT $
        rawQuery "SELECT full_name from lower_case_table WHERE my_id=1" []
          C.$$ CL.consume
      liftIO $ value `shouldBe` [[PersistText "cba"]]


asIO :: IO a -> IO a
asIO = id


