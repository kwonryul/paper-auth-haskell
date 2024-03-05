{-# LANGUAGE OverloadedStrings #-}

module JWT.Repository(
    JWTRepositoryI(
        verifyIdPw
      , getPreAuthenticatedUser
      , newAccessToken
      , newRefreshToken
      , saveAccessToken
      , saveRefreshToken
      , findByAccessTokenId
      , findByRefreshTokenId
      , invalidateJWT
      )
) where

import JWT.Model
import JWT.Entity
import User.Entity
import Role.Entity
import UserRole.Entity
import DB
import PaperMonad
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

class PaperMonadI p => JWTRepositoryI p where
    verifyIdPw :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> String -> String -> PaperMonad p m (Entity User)
    verifyIdPw = verifyIdPwImpl
    getPreAuthenticatedUser :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> UserId -> PaperMonad p m PreAuthenticatedUser
    getPreAuthenticatedUser = getPreAuthenticatedUserImpl
    newAccessToken :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> UserId -> UTCTime -> Maybe UTCTime -> RefreshTokenId -> PaperMonad p m AccessTokenId
    newAccessToken = newAccessTokenImpl
    newRefreshToken :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> UserId -> UTCTime -> Maybe UTCTime -> PaperMonad p m RefreshTokenId
    newRefreshToken = newRefreshTokenImpl
    saveAccessToken :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> AccessTokenId -> Text -> PaperMonad p m ()
    saveAccessToken = saveAccessTokenImpl
    saveRefreshToken :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> RefreshTokenId -> Text -> PaperMonad p m ()
    saveRefreshToken = saveRefreshTokenImpl
    findByAccessTokenId :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> AccessTokenId -> PaperMonad p m (Maybe AccessToken)
    findByAccessTokenId = findByAccessTokenIdImpl
    findByRefreshTokenId :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> RefreshTokenId -> PaperMonad p m (Maybe RefreshToken)
    findByRefreshTokenId = findByRefreshTokenIdImpl
    invalidateJWT :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> UserId -> PaperMonad p m ()
    invalidateJWT = invalidateJWTImpl

verifyIdPwImpl :: forall p m. (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> String -> String -> PaperMonad p m (Entity User)
verifyIdPwImpl conn paperId password = do
    userEntity' <- paperLiftUnliftIO $ runReaderT (getBy $ UniquePaperId paperId) conn
    rt@(Entity _ user) <- case userEntity' of
        Just userEntity -> return userEntity
        Nothing ->
            toPaperMonad $ PaperError "user not found" (err401 { errBody = "user not found" }) (callStack' profile)
    paperAssert
        (validatePassword (userPassword user) (Data.ByteString.Char8.pack password))
        (PaperError "password invalid" (err401 { errBody = "password invalid" }) (callStack' profile))
    return rt
    where
        profile :: Proxy p
        profile = Proxy

getPreAuthenticatedUserImpl :: forall p m. (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> UserId -> PaperMonad p m PreAuthenticatedUser
getPreAuthenticatedUserImpl conn userId = do
    userEntity' <- paperLiftUnliftIO $ runReaderT (get userId) conn
    _ <- maybeToPaperMonad userEntity' $ PaperError "user not found" (err500 { errBody = "user not found" }) (callStack' profile)
    userRoleEntityList <- paperLiftUnliftIO $ runReaderT (selectList [UserRoleUserId ==. userId] []) conn
    let roleIdList = (\(Entity _ userRole) -> userRoleRoleId userRole) <$> userRoleEntityList
    roleEntityList <- paperLiftUnliftIO $ runReaderT (selectList [RoleId <-. roleIdList] []) conn
    let roleSet = fromList $ (\(Entity _ role) -> role) <$> roleEntityList
    return $ PreAuthenticatedUser userId roleSet
    where
        profile :: Proxy p
        profile = Proxy

newAccessTokenImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> UserId -> UTCTime -> Maybe UTCTime -> RefreshTokenId -> PaperMonad p m AccessTokenId
newAccessTokenImpl conn userId iat exp' refreshTokenId =
    paperLiftUnliftIO $ runReaderT (Database.Persist.Sql.insert $ AccessToken Nothing userId iat exp' refreshTokenId) conn

newRefreshTokenImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> UserId -> UTCTime -> Maybe UTCTime -> PaperMonad p m RefreshTokenId
newRefreshTokenImpl conn userId iat exp' =
    paperLiftUnliftIO $ runReaderT (Database.Persist.Sql.insert $ RefreshToken Nothing userId iat exp') conn

saveAccessTokenImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> AccessTokenId -> Text -> PaperMonad p m ()
saveAccessTokenImpl conn accessTokenId accessToken =
    paperLiftUnliftIO $ runReaderT (update accessTokenId [AccessTokenToken =. Just accessToken]) conn

saveRefreshTokenImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> RefreshTokenId -> Text -> PaperMonad p m ()
saveRefreshTokenImpl conn refreshTokenId refreshToken =
    paperLiftUnliftIO $ runReaderT (update refreshTokenId [RefreshTokenToken =. Just refreshToken]) conn

findByAccessTokenIdImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> AccessTokenId -> PaperMonad p m (Maybe AccessToken)
findByAccessTokenIdImpl conn accessTokenId =
    paperLiftUnliftIO $ runReaderT (get accessTokenId) conn

findByRefreshTokenIdImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> RefreshTokenId -> PaperMonad p m (Maybe RefreshToken)
findByRefreshTokenIdImpl conn refreshTokenId =
    paperLiftUnliftIO $ runReaderT (get refreshTokenId) conn

invalidateJWTImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> UserId -> PaperMonad p m ()
invalidateJWTImpl conn userId = do
    paperLiftUnliftIO $ runReaderT (do
        deleteWhere [AccessTokenUserId ==. userId]
        deleteWhere [RefreshTokenUserId ==. userId]
        ) conn