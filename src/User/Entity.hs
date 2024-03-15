{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module User.Entity where

import DB
import Enum

import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Typed

import Data.ByteString
import Data.Time

share [mkPersist (mkSqlSettingsFor ''PaperAuthDB), mkMigrate "migrateUser"] [persistLowerCase|
User
    authenticationType AuthenticationType
    paperId String
    password ByteString Maybe
    name String
    phoneNumber String Maybe
    registerDate UTCTime
    UniquePaperId paperId
|]