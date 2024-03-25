{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lock.Entity where

import DB

import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Typed

share  [mkPersist (mkSqlSettingsFor ''PaperAuthDB), mkMigrate "migrateLock"] [persistLowerCase|
Lock
    name String
|]