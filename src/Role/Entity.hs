{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Role.Entity where

import DB

import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Typed

share [mkPersist (mkSqlSettingsFor ''PaperAuthDB), mkMigrate "migrateRole"] [persistLowerCase|
Role
    name String
    UniqueName name
    deriving Eq Ord
|]