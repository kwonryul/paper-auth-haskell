{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UserRole.Entity where

import User.Entity
import Role.Entity
import DB

import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Typed

share [mkPersist (mkSqlSettingsFor ''PaperAuthDB), mkMigrate "migrateUserRole"] [persistLowerCase|
UserRole
    userId UserId
    roleId RoleId
    deriving Show Eq
|]