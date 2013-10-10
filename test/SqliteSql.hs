{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module SqliteSql where

import Text.Shakespeare.Text
import Database.Persist.Migrationplus

tableIdTrig :: SqlUnit
tableIdTrig = ("tableIdTrig",[lt|
      INSERT INTO ref_table (something_else,lct) SELECT my_id,1 FROM lower_case_table ORDER BY my_id DESC LIMIT 1;
    |])

tableTrig :: SqlUnit
tableTrig = ("tableTrig",[lt|
      UPDATE lower_case_table SET full_name = "abc" WHERE id = 1;
    |])

sql :: [SqlUnit]
sql = [tableTrig, tableIdTrig]


