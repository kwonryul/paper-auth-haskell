{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module JWT.Entity where

import DB
import User.Entity

import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Typed

import Data.Text
import Data.Time

share [mkPersist (mkSqlSettingsFor ''PaperAuthDB), mkMigrate "migrateJWT"] [persistLowerCase|
AccessToken
    token Text Maybe
    userId UserId
    iat UTCTime
    expire UTCTime Maybe
    refreshTokenId RefreshTokenId

RefreshToken
    token Text Maybe
    userId UserId
    iat UTCTime
    expire UTCTime Maybe
|]