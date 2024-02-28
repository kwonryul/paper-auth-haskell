{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module JWT.Service(
    issueJWT
  , refreshJWT
  , invalidateJWT
) where

import qualified JWT.Repository
import qualified Verification.Service

import JWT.Util
import JWT.DTO
import JWT.Model
import User.Entity
import DB
import PaperError

import Servant
import Database.Persist.Sql
import Database.Persist.Typed
import Web.JWT
import Web.Cookie
import Data.Configurator.Types

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Time.Clock
import GHC.Stack

issueJWT :: (HasCallStack, MonadUnliftIO m) => Config -> PaperAuthPool -> EncodeSigner -> String -> String -> PaperExceptT m (Headers '[Header "Set-Cookie" SetCookie] IssueJWTResDTO)
issueJWT config pool encodeSigner paperId password = do
    unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\conn ->
        catchE (inner conn) $ \e -> do
            paperLift $ runReaderT transactionUndo (generalizeSqlBackend conn)
            ExceptT $ return $ Left e
            )) pool
    where
        inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PaperExceptT m (Headers '[Header "Set-Cookie" SetCookie] IssueJWTResDTO)
        inner conn = issueJWT' config conn encodeSigner paperId password

issueJWT' :: (HasCallStack, MonadUnliftIO m) => Config -> PaperAuthConn -> EncodeSigner -> String -> String -> PaperExceptT m (Headers '[Header "Set-Cookie" SetCookie] IssueJWTResDTO)
issueJWT' config conn encodeSigner paperId password = do
    currentUTC <- paperLiftIO getCurrentTime
    Entity userId _ <- JWT.Repository.verifyIdPw conn paperId password
    preAuthenticatedUser <- JWT.Repository.getPreAuthenticatedUser conn userId
    JWTDTO { accessToken, refreshToken } <- Verification.Service.issueJWT config conn encodeSigner preAuthenticatedUser currentUTC
    let cookie = generateRefreshTokenCookie refreshToken
    return $ addHeader cookie $ IssueJWTResDTO { accessToken }

refreshJWT :: (HasCallStack, MonadUnliftIO m) => Config -> PaperAuthPool -> EncodeSigner -> UserId -> PaperExceptT m (Headers '[Header "Set-Cookie" SetCookie] RefreshJWTResDTO)
refreshJWT config pool encodeSigner userId = do
    unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\conn ->
        catchE (inner conn) $ \e -> do
            paperLift $ runReaderT transactionUndo (generalizeSqlBackend conn)
            ExceptT $ return $ Left e
            )) pool
    where
        inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PaperExceptT m (Headers '[Header "Set-Cookie" SetCookie] RefreshJWTResDTO)
        inner conn = refreshJWT' config conn encodeSigner userId

refreshJWT' :: (HasCallStack, MonadUnliftIO m) => Config -> PaperAuthConn -> EncodeSigner -> UserId -> PaperExceptT m (Headers '[Header "Set-Cookie" SetCookie] RefreshJWTResDTO)
refreshJWT' config conn encodeSigner userId = do
    currentUTC <- paperLiftIO getCurrentTime
    preAuthenticatedUser <- JWT.Repository.getPreAuthenticatedUser conn userId
    JWTDTO { accessToken, refreshToken } <- Verification.Service.issueJWT config conn encodeSigner preAuthenticatedUser currentUTC
    let cookie = generateRefreshTokenCookie refreshToken
    return $ addHeader cookie $ RefreshJWTResDTO { accessToken }

invalidateJWT :: (HasCallStack, MonadUnliftIO m) => PaperAuthPool -> UserId -> PaperExceptT m NoContent
invalidateJWT pool userId = do
    unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\conn ->
        catchE (inner conn) $ \e -> do
            paperLift $ runReaderT transactionUndo (generalizeSqlBackend conn)
            ExceptT $ return $ Left e
            )) pool
    where
        inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PaperExceptT m NoContent
        inner conn = invalidateJWT' conn userId

invalidateJWT' :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> UserId -> PaperExceptT m NoContent
invalidateJWT' conn userId = do
    Verification.Service.invalidateJWT conn userId
    return NoContent