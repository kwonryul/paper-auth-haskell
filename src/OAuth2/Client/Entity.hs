{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module OAuth2.Client.Entity where

import DB
import Enum

import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Typed

import Data.Time
import Data.Text

share [mkPersist (mkSqlSettingsFor ''PaperAuthDB), mkMigrate "migrateOAuth2ClientSocket"] [persistLowerCase|
OAuth2ClientSocket
    type SocketType
    host String
    port Int
    state Text Maybe
    iat UTCTime
|]