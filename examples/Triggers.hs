{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Triggers where

import Text.Shakespeare.Text
import Data.Text.Lazy (Text)

tableIdTrig :: Text
tableIdTrig = [lt|
    CREATE OR REPLACE FUNCTION tableIdTrig()
      RETURNS trigger AS
    $BODY$
      BEGIN
        PERFORM pg_notify(TG_TABLE_NAME,NEW.id::TEXT);
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
triggers = [tableTrig, tableIdTrig]


