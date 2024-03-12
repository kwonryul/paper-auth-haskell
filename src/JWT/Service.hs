{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module JWT.Service(
    JWTServiceI(
        issueJWT
      , refreshJWT
      , invalidateJWT
      )
) where

import qualified JWT.ExService
import JWT.ExService(JWTExServiceI)
import qualified User.Repository
import User.Repository(UserRepositoryI)

import JWT.Util
import JWT.DTO
import JWT.Model
import User.Entity
import DB
import PaperMonad

import Servant
import Database.Persist.Sql
import Web.JWT
import Web.Cookie
import Data.Configurator.Types

import Control.Monad.IO.Unlift
import Data.Time.Clock
import GHC.Stack

class (DBI p, JWTUtilI p, UserRepositoryI p, JWTExServiceI p) => JWTServiceI p where
    issueJWT :: (HasCallStack, MonadUnliftIO m) => Config -> EncodeSigner -> String -> String -> PaperAuthPool -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] IssueJWTResDTO)
    issueJWT = issueJWTImpl
    issueJWT' :: (HasCallStack, MonadUnliftIO m) => Config -> EncodeSigner -> String -> String -> PaperAuthConn -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] IssueJWTResDTO)
    issueJWT' = issueJWT'Impl
    refreshJWT :: (HasCallStack, MonadUnliftIO m) => Config -> EncodeSigner -> UserId -> PaperAuthPool -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] RefreshJWTResDTO)
    refreshJWT = refreshJWTImpl
    refreshJWT' :: (HasCallStack, MonadUnliftIO m) => Config -> EncodeSigner -> UserId -> PaperAuthConn -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] RefreshJWTResDTO)
    refreshJWT' = refreshJWT'Impl
    invalidateJWT :: (HasCallStack, MonadUnliftIO m) => UserId -> PaperAuthPool -> PaperMonad p m NoContent
    invalidateJWT = invalidateJWTImpl
    invalidateJWT' :: (HasCallStack, MonadUnliftIO m) => UserId -> PaperAuthConn -> PaperMonad p m NoContent
    invalidateJWT' = invalidateJWT'Impl

issueJWTImpl :: (HasCallStack, JWTServiceI p, MonadUnliftIO m) => Config -> EncodeSigner -> String -> String -> PaperAuthPool -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] IssueJWTResDTO)
issueJWTImpl config encodeSigner paperId password pool = runSqlPoolOneConnection (issueJWT' config encodeSigner paperId password) pool

issueJWT'Impl :: forall p m. (HasCallStack, JWTServiceI p, MonadUnliftIO m) => Config -> EncodeSigner -> String -> String -> PaperAuthConn -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] IssueJWTResDTO)
issueJWT'Impl config encodeSigner paperId password conn = do
    currentUTC <- paperLiftIOUnliftIO getCurrentTime
    Entity userId _ <- User.Repository.verifyIdPw conn paperId password
    preAuthenticatedUser <- User.Repository.getPreAuthenticatedUser conn userId
    JWTDTO { accessToken, refreshToken } <- JWT.ExService.issueJWT config conn encodeSigner preAuthenticatedUser currentUTC
    let cookie = generateRefreshTokenCookie (Proxy :: Proxy p) refreshToken
    return $ addHeader cookie $ IssueJWTResDTO { accessToken }

refreshJWTImpl :: (HasCallStack, JWTServiceI p, MonadUnliftIO m) => Config -> EncodeSigner -> UserId -> PaperAuthPool -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] RefreshJWTResDTO)
refreshJWTImpl config encodeSigner userId pool = runSqlPoolOneConnection (refreshJWT' config encodeSigner userId) pool

refreshJWT'Impl :: forall p m. (HasCallStack, JWTServiceI p, MonadUnliftIO m) => Config -> EncodeSigner -> UserId -> PaperAuthConn -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] RefreshJWTResDTO)
refreshJWT'Impl config encodeSigner userId conn = do
    currentUTC <- paperLiftIOUnliftIO getCurrentTime
    preAuthenticatedUser <- User.Repository.getPreAuthenticatedUser conn userId
    JWTDTO { accessToken, refreshToken } <- JWT.ExService.issueJWT config conn encodeSigner preAuthenticatedUser currentUTC
    let cookie = generateRefreshTokenCookie (Proxy :: Proxy p) refreshToken
    return $ addHeader cookie $ RefreshJWTResDTO { accessToken }

invalidateJWTImpl :: (HasCallStack, JWTServiceI p, MonadUnliftIO m) => UserId -> PaperAuthPool -> PaperMonad p m NoContent
invalidateJWTImpl userId pool = runSqlPoolOneConnection (invalidateJWT' userId) pool

invalidateJWT'Impl :: (HasCallStack, JWTServiceI p, MonadUnliftIO m) => UserId -> PaperAuthConn -> PaperMonad p m NoContent
invalidateJWT'Impl userId conn = do
    JWT.ExService.invalidateJWT conn userId
    return NoContent