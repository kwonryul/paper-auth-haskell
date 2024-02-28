{-# LANGUAGE OverloadedStrings #-}

module JWT.Repository(
    verifyIdPw
  , getPreAuthenticatedUser
  , newAccessToken
  , newRefreshToken
  , saveAccessToken
  , saveRefreshToken
  , findByAccessTokenId
  , findByRefreshTokenId
  , invalidateJWT
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

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Set
import Data.Text
import Data.Time
import GHC.Stack

import Crypto.BCrypt
import Data.ByteString.Char8

verifyIdPw :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> String -> String -> PaperExceptT m (Entity User)
verifyIdPw conn paperId password = do
    userEntity' <- paperLift $ runReaderT (getBy $ UniquePaperId paperId) conn
    rt@(Entity _ user) <- case userEntity' of
        Just userEntity -> return userEntity
        Nothing ->
            toPaperExceptT $ PaperException "user not found" (err401 { errBody = "user not found" }) callStack'
    paperAssert
        (validatePassword (userPassword user) (Data.ByteString.Char8.pack password))
        (PaperException "password invalid" (err401 { errBody = "password invalid" }) callStack')
    return rt

getPreAuthenticatedUser :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> UserId -> PaperExceptT m PreAuthenticatedUser
getPreAuthenticatedUser conn userId = do
    userEntity' <- paperLift $ runReaderT (get userId) conn
    _ <- maybeToPaperExceptT userEntity' $ PaperException "user not found" (err500 { errBody = "user not found" }) callStack'
    userRoleEntityList <- paperLift $ runReaderT (selectList [UserRoleUserId ==. userId] []) conn
    let roleIdList = (\(Entity _ userRole) -> userRoleRoleId userRole) <$> userRoleEntityList
    roleEntityList <- paperLift $ runReaderT (selectList [RoleId <-. roleIdList] []) conn
    let roleSet = fromList $ (\(Entity _ role) -> role) <$> roleEntityList
    return $ PreAuthenticatedUser userId roleSet

newAccessToken :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> UserId -> UTCTime -> Maybe UTCTime -> RefreshTokenId -> PaperExceptT m AccessTokenId
newAccessToken conn userId iat exp' refreshTokenId =
    paperLift $ runReaderT (Database.Persist.Sql.insert $ AccessToken Nothing userId iat exp' refreshTokenId) conn

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

findByRefreshTokenId :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> RefreshTokenId -> PaperExceptT m (Maybe RefreshToken)
findByRefreshTokenId conn refreshTokenId =
    paperLift $ runReaderT (get refreshTokenId) conn

invalidateJWT :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> UserId -> PaperExceptT m ()
invalidateJWT conn userId = do
    paperLift $ runReaderT (do
        deleteWhere [AccessTokenUserId ==. userId]
        deleteWhere [RefreshTokenUserId ==. userId]
        ) conn