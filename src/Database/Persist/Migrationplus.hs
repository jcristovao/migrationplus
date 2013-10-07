{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Migrationplus
 ( ExtraValidate
 , doesNotSupport
 , ExtraCapabilities(..)
 , validateExtras'
 ) where

import Database.Persist.Quasi

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text (Text)

-- | Extra validate function
type ExtraValidate = [LT.Text] -> [[Text]] -> Bool

-- | Handy for signaling that a backend does not support the given feature
doesNotSupport :: ExtraValidate
doesNotSupport = const (const False)

-- | Validation capabilites of given backend
data ExtraCapabilities = ExtraCapabilities
  { valTriggers :: ExtraValidate
  , valIndexes  :: ExtraValidate
  -- more to be added in the future
  -- ... or contribute your own!
  }

capabilities :: [(Text, ExtraCapabilities -> ExtraValidate)]
capabilities = [ ("Triggers", valTriggers)
               , ("Indexes" , valIndexes )
               ]

-- | Validate extras: generic function that handles provided backend
-- validation functions. It only evaluates them if a given extra feature
-- was requested in the database definition.
validateExtras' :: ExtraCapabilities -> ValidateExtras LT.Text
validateExtras' ecap sql extras = let
  lookupExtras capName  = Map.lookup capName extras
  test (capName,capVal) =  (maybe True (capVal ecap $ sql) $ lookupExtras capName)
                        || (error $  "Invalid " ++ T.unpack capName ++ ": "
                                 ++ ( show
                                    $ maybe [] concat $ lookupExtras capName)
                           )
  in if all test capabilities
       then extras
       else error "Invalid extra persistent settings"


