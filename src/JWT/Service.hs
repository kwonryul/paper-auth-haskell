{-# LANGUAGE OverloadedStrings #-}

module JWT.Service(
    issueJWT
) where

import qualified JWT.Repository
import qualified Verification.Service

import JWT.DTO
import DB
import PaperError

import Database.Persist.Sql
import Database.Persist.Typed
import Web.JWT
import Data.Configurator.Types

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Time.Clock
import GHC.Stack

issueJWT :: (HasCallStack, MonadUnliftIO m) => Config -> PaperAuthPool -> EncodeSigner -> String -> String -> PaperExceptT m IssueJWTResDTO
issueJWT config pool encodeSigner paperId password = do
    unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\conn ->
        catchE (inner conn) $ \e -> do
            paperLift $ runReaderT transactionUndo (generalizeSqlBackend conn)
            ExceptT $ return $ Left e
            )) pool
    where
        inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PaperExceptT m IssueJWTResDTO
        inner conn = issueJWT' config conn encodeSigner paperId password

issueJWT' :: (HasCallStack, MonadUnliftIO m) => Config -> PaperAuthConn -> EncodeSigner -> String -> String -> PaperExceptT m IssueJWTResDTO
issueJWT' config conn encodeSigner paperId password = do
    authenticatedUser <- JWT.Repository.getAuthenticatedUser conn paperId password
    currentUTC <- paperLiftIO getCurrentTime
    jwtDTO <- Verification.Service.issueJWT config conn encodeSigner authenticatedUser currentUTC
    return $ fromJWTDTO jwtDTO