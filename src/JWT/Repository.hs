{-# LANGUAGE OverloadedStrings #-}

module JWT.Repository(
    JWTRepositoryI(
        newAccessToken
      , newRefreshToken
      , saveAccessToken
      , saveRefreshToken
      , findByAccessTokenId
      , findByRefreshTokenId
      , invalidateJWT
      )
) where

import JWT.Entity
import User.Entity
import DB
import PaperMonad

import Database.Persist.Sql

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Text
import Data.Time
import GHC.Stack

class PaperMonadI p => JWTRepositoryI p where
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

newAccessTokenImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> UserId -> UTCTime -> Maybe UTCTime -> RefreshTokenId -> PaperMonad p m AccessTokenId
newAccessTokenImpl conn userId iat exp' refreshTokenId =
    paperLiftUnliftIO $ runReaderT (insert $ AccessToken Nothing userId iat exp' refreshTokenId) conn

newRefreshTokenImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> UserId -> UTCTime -> Maybe UTCTime -> PaperMonad p m RefreshTokenId
newRefreshTokenImpl conn userId iat exp' =
    paperLiftUnliftIO $ runReaderT (insert $ RefreshToken Nothing userId iat exp') conn

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