{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module PgsqlTriggers where

import Text.Shakespeare.Text
import Data.Text.Lazy (Text)

tableIdTrig :: Text
tableIdTrig = [lt|
    CREATE OR REPLACE FUNCTION tableIdTrig()
      RETURNS trigger AS
    $BODY$
      BEGIN
        PERFORM pg_notify(TG_TABLE_NAME,NEW.my_id::TEXT);
        RETURN NULL;
      END
    $BODY$
      LANGUAGE plpgsql VOLATILE;
    |]

tableChgIns :: Text
tableChgIns= [lt|
    CREATE OR REPLACE FUNCTION tableChgIns()
      RETURNS trigger AS
    $BODY$
      BEGIN
        UPDATE lower_case_table SET full_name = 'cba' WHERE my_id = (SELECT my_id FROM lower_case_table ORDER BY my_id DESC LIMIT 1);
        RETURN NULL;
      END
    $BODY$
      LANGUAGE plpgsql VOLATILE;
    |]


tableTrig :: Text
tableTrig = [lt|
    CREATE OR REPLACE FUNCTION tableTrig()
      RETURNS trigger AS
    $BODY$
      BEGIN
        PERFORM pg_notify(TG_TABLE_NAME,TG_TABLE_NAME);
        RETURN NULL;
      END;
    $BODY$
      LANGUAGE plpgsql VOLATILE;
    |]

triggers :: [Text]
triggers = [tableTrig, tableIdTrig, tableChgIns]


