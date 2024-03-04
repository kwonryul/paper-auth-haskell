{-# LANGUAGE DuplicateRecordFields #-}

module JWT.Model(
    PreAuthenticatedUser(
        PreAuthenticatedUser
      , userId
      , roleSet
    )
  , AuthenticatedUser(
        AuthenticatedUser
      , accessTokenId
      , refreshTokenId
      , userId
      , roleSet
      )
  , AuthenticatedUserRefresh(
        AuthenticatedUserRefresh
      , refreshTokenId
      , userId
      )
  , JWTDTO(
        JWTDTO
      , accessToken
      , refreshToken
      , accessTokenId
      , refreshTokenId
      )
) where

import JWT.Entity
import User.Entity
import Role.Entity

import Data.Set
import Data.Text

data PreAuthenticatedUser = PreAuthenticatedUser {
    userId :: UserId
  , roleSet :: Set Role
  }

data AuthenticatedUser = AuthenticatedUser {
    accessTokenId :: AccessTokenId
  , refreshTokenId :: RefreshTokenId
  , userId :: UserId
  , roleSet :: Set Role
  }

data AuthenticatedUserRefresh = AuthenticatedUserRefresh {
    refreshTokenId :: RefreshTokenId
  , userId :: UserId
  }

data JWTDTO = JWTDTO {
    accessTokenId :: AccessTokenId
  , accessToken :: Text
  , refreshTokenId :: RefreshTokenId
  , refreshToken :: Text
  }