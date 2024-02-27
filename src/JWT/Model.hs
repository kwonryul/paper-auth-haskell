module JWT.Model(
    AuthenticationType(
      TypePaper
    )
  , AuthenticatedUser(
        AuthenticatedUser
      , userId
      , roleSet
      )
  , AuthenticatedUserCsrf(AuthenticatedUserCsrf)
  , JWTDTO(
        JWTDTO
      , accessToken
      , refreshToken
      , csrfToken
      )
  , FromJWTDTO(
        fromJWTDTO
      )
) where

import User.Entity
import Role.Entity

import Data.Set
import Data.Text

data AuthenticationType = TypePaper
  deriving (Show, Eq)

data AuthenticatedUser = AuthenticatedUser {
    userId :: UserId
  , roleSet :: Set Role
}

newtype AuthenticatedUserCsrf = AuthenticatedUserCsrf AuthenticatedUser

data JWTDTO = JWTDTO {
    accessToken :: Text
  , refreshToken :: Text
  , csrfToken :: Text
  }

class FromJWTDTO a where
    fromJWTDTO :: JWTDTO -> a
