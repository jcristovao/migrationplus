{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
module RenameTest (specs) where

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#elif WITH_MYSQL
import Database.Persist.MySQL
#else
import Database.Persist.Sqlite
#endif
import qualified Data.Map as Map
import qualified Data.Text as T

import Init
import MigrationSql
import Database.Persist.Postgresql.Migrationplus

-- Test lower case names
#if WITH_MONGODB
mkPersist persistSettings [persistLowerCase|
#else
share [mkPersist sqlSettings, mkMigrate "lowerCaseMigrate"] [persistLowerWithSql|
#endif
LowerCaseTable id=my_id
    fullName Text
    Triggers
        tableIdTrig AFTER INSERT OR UPDATE
        tableTrig BEFORE DELETE UPDATE
RefTable
    someVal Int sql=something_else
    lct LowerCaseTableId
    UniqueRefTable someVal
|]

specs :: Spec
specs = describe "rename specs" $ do
    it "handles lower casing" $ asIO $ do
        runConn' (getSqlCode,triggers) $ do
            _ <- runMigration lowerCaseMigrate
            C.runResourceT $ rawQuery "SELECT full_name from lower_case_table WHERE my_id=5" [] C.$$ CL.sinkNull
            C.runResourceT $ rawQuery "SELECT something_else from ref_table WHERE id=4" [] C.$$ CL.sinkNull
    it "extra blocks" $ do
        entityExtra (entityDef (Nothing :: Maybe LowerCaseTable)) @?=
            Map.fromList
                [ ("Triggers", map T.words ["tableIdTrig AFTER INSERT OR UPDATE","tableTrig BEFORE DELETE UPDATE"])
                ]

asIO :: IO a -> IO a
asIO = id

