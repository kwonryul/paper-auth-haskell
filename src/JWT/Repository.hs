{-# LANGUAGE OverloadedStrings #-}

module JWT.Repository(
    getAuthenticatedUser
  , newAccessToken
  , newRefreshToken
  , saveAccessToken
  , saveRefreshToken
  , findByAccessTokenId
) where

import JWT.Model
import JWT.Entity
import User.Entity
import Role.Entity
import UserRole.Entity
import DB
import PaperError
import CallStack

import Servant
import Database.Persist.Sql
import Crypto.BCrypt

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Set
import Data.Text
import Data.Time
import Data.ByteString.Char8
import GHC.Stack

getAuthenticatedUser :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> String -> String -> PaperExceptT m AuthenticatedUser
getAuthenticatedUser conn paperId password = do
    userEntityList <- paperLift $ runReaderT (selectList [UserPaperId ==. paperId] []) conn
    (userId, user) <- case userEntityList of
        (Entity userId user : []) ->
            return (userId, user)
        (_ : _ : _) ->
            toPaperExceptT $ PaperException "paperId duplicate" (err500 { errBody = "Internal server error" }) callStack'
        [] ->
            toPaperExceptT $ PaperException "user not found" (err401 { errBody = "user not found" }) callStack'
    paperAssert
        (validatePassword (userPassword user) (Data.ByteString.Char8.pack password))
        (PaperException "password invalid" (err401 { errBody = "password invalid" }) callStack')
    userRoleEntityList <- paperLift $ runReaderT (selectList [UserRoleUserId ==. userId] []) conn
    let roleIdList = (\(Entity _ userRole) -> userRoleRoleId userRole) <$> userRoleEntityList
    roleEntityList <- paperLift $ runReaderT (selectList [RoleId <-. roleIdList] []) conn
    let roleSet = fromList $ (\(Entity _ role) -> role) <$> roleEntityList
    return $ AuthenticatedUser userId roleSet

newAccessToken :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> UserId -> UTCTime -> Maybe UTCTime -> Text -> PaperExceptT m AccessTokenId
newAccessToken conn userId iat exp' csrfToken =
    paperLift $ runReaderT (Database.Persist.Sql.insert $ AccessToken Nothing userId iat exp' csrfToken) conn

newRefreshToken :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> UserId -> UTCTime -> Maybe UTCTime -> PaperExceptT m RefreshTokenId
newRefreshToken conn userId iat exp' =
    paperLift $ runReaderT (Database.Persist.Sql.insert $ RefreshToken Nothing userId iat exp') conn

saveAccessToken :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> AccessTokenId -> Text -> PaperExceptT m ()
saveAccessToken conn accessTokenId accessToken =
    paperLift $ runReaderT (update accessTokenId [AccessTokenToken =. Just accessToken]) conn

saveRefreshToken :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> RefreshTokenId -> Text -> PaperExceptT m ()
saveRefreshToken conn refreshTokenId refreshToken =
    paperLift $ runReaderT (update refreshTokenId [RefreshTokenToken =. Just refreshToken]) conn

findByAccessTokenId :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> AccessTokenId -> PaperExceptT m (Maybe AccessToken)
findByAccessTokenId conn accessTokenId =
    paperLift $ runReaderT (get accessTokenId) conn