{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module User.Entity where

import DB

import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Typed

share [mkPersist (mkSqlSettingsFor ''PaperAuthDB), mkMigrate "migrateUser"] [persistLowerCase|
User
    paperId String
    password String
    name String
    deriving Show Eq
|]