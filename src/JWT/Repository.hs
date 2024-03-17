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
    newAccessToken :: (HasCallStack, MonadUnliftIO m) => UserId -> UTCTime -> Maybe UTCTime -> RefreshTokenId -> PaperAuthConn -> PaperMonad p m AccessTokenId
    newAccessToken = newAccessTokenImpl
    newRefreshToken :: (HasCallStack, MonadUnliftIO m) => UserId -> UTCTime -> Maybe UTCTime -> PaperAuthConn -> PaperMonad p m RefreshTokenId
    newRefreshToken = newRefreshTokenImpl
    saveAccessToken :: (HasCallStack, MonadUnliftIO m) => AccessTokenId -> Text -> PaperAuthConn -> PaperMonad p m ()
    saveAccessToken = saveAccessTokenImpl
    saveRefreshToken :: (HasCallStack, MonadUnliftIO m) => RefreshTokenId -> Text -> PaperAuthConn -> PaperMonad p m ()
    saveRefreshToken = saveRefreshTokenImpl
    findByAccessTokenId :: (HasCallStack, MonadUnliftIO m) => AccessTokenId -> PaperAuthConn -> PaperMonad p m (Maybe AccessToken)
    findByAccessTokenId = findByAccessTokenIdImpl
    findByRefreshTokenId :: (HasCallStack, MonadUnliftIO m) => RefreshTokenId -> PaperAuthConn -> PaperMonad p m (Maybe RefreshToken)
    findByRefreshTokenId = findByRefreshTokenIdImpl
    invalidateJWT :: (HasCallStack, MonadUnliftIO m) => UserId -> PaperAuthConn -> PaperMonad p m ()
    invalidateJWT = invalidateJWTImpl

newAccessTokenImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => UserId -> UTCTime -> Maybe UTCTime -> RefreshTokenId -> PaperAuthConn -> PaperMonad p m AccessTokenId
newAccessTokenImpl userId iat exp' refreshTokenId conn =
    paperLiftUnliftIO $ runReaderT (insert $ AccessToken Nothing userId iat exp' refreshTokenId) conn

newRefreshTokenImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => UserId -> UTCTime -> Maybe UTCTime -> PaperAuthConn -> PaperMonad p m RefreshTokenId
newRefreshTokenImpl userId iat exp' conn =
    paperLiftUnliftIO $ runReaderT (insert $ RefreshToken Nothing userId iat exp') conn

saveAccessTokenImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => AccessTokenId -> Text -> PaperAuthConn -> PaperMonad p m ()
saveAccessTokenImpl accessTokenId accessToken conn =
    paperLiftUnliftIO $ runReaderT (update accessTokenId [AccessTokenToken =. Just accessToken]) conn

saveRefreshTokenImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => RefreshTokenId -> Text -> PaperAuthConn -> PaperMonad p m ()
saveRefreshTokenImpl refreshTokenId refreshToken conn =
    paperLiftUnliftIO $ runReaderT (update refreshTokenId [RefreshTokenToken =. Just refreshToken]) conn

findByAccessTokenIdImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => AccessTokenId -> PaperAuthConn -> PaperMonad p m (Maybe AccessToken)
findByAccessTokenIdImpl accessTokenId conn =
    paperLiftUnliftIO $ runReaderT (get accessTokenId) conn

findByRefreshTokenIdImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => RefreshTokenId -> PaperAuthConn -> PaperMonad p m (Maybe RefreshToken)
findByRefreshTokenIdImpl refreshTokenId conn =
    paperLiftUnliftIO $ runReaderT (get refreshTokenId) conn

invalidateJWTImpl :: (HasCallStack, JWTRepositoryI p, MonadUnliftIO m) => UserId -> PaperAuthConn -> PaperMonad p m ()
invalidateJWTImpl userId conn = do
    paperLiftUnliftIO $ runReaderT (do
        deleteWhere [AccessTokenUserId ==. userId]
        deleteWhere [RefreshTokenUserId ==. userId]
        ) conn