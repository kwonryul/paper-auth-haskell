module JWT.Model(
    AuthenticatedUser(
        AuthenticatedUser
      , authenticatedUserId
      , authenticatedRoleSet
      )
) where

import User.Entity
import Role.Entity

import Data.Set
import GHC.Generics

data AuthenticatedUser = AuthenticatedUser {
    authenticatedUserId :: UserId
  , authenticatedRoleSet :: Set Role
} deriving (Show, Generic)