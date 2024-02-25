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

import Data.Time

share [mkPersist (mkSqlSettingsFor ''PaperAuthDB), mkMigrate "migrateJWT"] [persistLowerCase|
AccessToken
    token String
    userId UserId
    expire UTCTime
    csrfToken String
    deriving Show Eq

RefreshToken
    token String
    userId UserId
    expire UTCTime
    deriving Show Eq
|]

{-}
import Servant.Auth.Server

import GHC.Generics
import Data.Aeson

data AuthenticatedUser = AuthenticatedUser {
    auUserId :: Int
  , auRoles :: [AuthenticatedUserRole]
  } deriving (Show, Generic)

data AuthenticatedUserRole =
    Admin
  | Premium
  deriving (Show, Generic)

instance ToJSON AuthenticatedUserRole
instance FromJSON AuthenticatedUserRole
instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

-}