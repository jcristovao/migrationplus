{-# LANGUAGE CPP #-}

import qualified RenameTest
import Test.Hspec (hspec)
import Test.Hspec.Runner
import Init
import System.Exit
import Control.Monad (unless, when)
import Filesystem (isFile, removeFile)
import Filesystem.Path.CurrentOS (fromText)
import Control.Monad.Trans.Resource (runResourceT)


import Database.Persist.Sql (printMigration, runMigrationUnsafe)

setup migration = do
  printMigration migration
  runMigrationUnsafe migration

toExitCode :: Bool -> ExitCode
toExitCode True  = ExitSuccess
toExitCode False = ExitFailure 1

main :: IO ()
main = do
  sqExists <- isFile $ fromText sqlite_database
  when sqExists $ removeFile $ fromText sqlite_database
  hspec $ do
    RenameTest.specs
