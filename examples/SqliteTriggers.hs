{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module SqliteTriggers where

import Text.Shakespeare.Text
import Database.Persist.Migrationplus

tableIdTrig :: SqlUnit
tableIdTrig = ("tableIdTrig",[lt|
      UPDATE lower_case_table SET full_name = "abc" WHERE id = 1;
    |])

tableTrig :: SqlUnit
tableTrig = ("tableTrig",[lt|
      UPDATE lower_case_table SET full_name = "abc" WHERE id = 1;
    |])

triggers :: [SqlUnit]
triggers = [tableTrig, tableIdTrig]


