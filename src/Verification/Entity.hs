{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Verification.Entity where

import DB

import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Typed

import Data.Time

share [mkPersist (mkSqlSettingsFor ''PaperAuthDB), mkMigrate "migrateVerification"] [persistLowerCase|
Verification
    phoneNumber String
    phoneNumberSecret String
    iat UTCTime
    expire UTCTime
    deleteAt UTCTime
    UniquePhoneNumber phoneNumber
|]