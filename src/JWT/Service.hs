{-# LANGUAGE OverloadedStrings #-}

module JWT.Service(
    issueJWT
) where

import JWT.DTO
import JWT.Model
import JWT.Repository
import User.Entity
import DB
import PaperError

import Servant
import Database.Persist.Sql
import Database.Persist.Typed
import Data.Aeson.Types
import Web.JWT

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader
import Data.Map
import Data.Set
import Data.Vector
import Data.Text
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Stack

issueJWT :: (HasCallStack, MonadUnliftIO m) => PaperAuthPool -> EncodeSigner -> String -> String -> PaperExceptT m IssueJWTResDTO
issueJWT pool encodeSigner paperId password = do
    unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\conn -> do
        issueJWT' conn encodeSigner paperId password
        )) pool

issueJWT' :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> EncodeSigner -> String -> String -> PaperExceptT m IssueJWTResDTO
issueJWT' conn encodeSigner paperId password = do
    AuthenticatedUser { authenticatedUserId, authenticatedRoleSet } <- JWT.Repository.getAuthenticatedUser conn paperId password
    currentUTC <- paperLiftIO getCurrentTime
    let currentNominal = diffUTCTime currentUTC $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
        accessLifetime = fromInteger 1800
        refreshLifetime = fromInteger 10800
        tokenHeader = JOSEHeader Nothing Nothing Nothing Nothing
        sub = stringOrURI $ pack $ show $ fromSqlKeyFor authenticatedUserId
        accessExp = numericDate (currentNominal + accessLifetime)
        refreshExp = numericDate (currentNominal + refreshLifetime)
        iat = numericDate currentNominal
        claimsMap = ClaimsMap $ Data.Map.fromList [("roles", Array $ Data.Vector.fromList $ (String . pack . show) <$> Data.Set.toList authenticatedRoleSet)]
        accessClaimsSet = JWTClaimsSet Nothing sub Nothing accessExp Nothing iat Nothing claimsMap
        refreshClaimsSet = JWTClaimsSet Nothing sub Nothing refreshExp Nothing iat Nothing claimsMap
        accessToken = encodeSigned encodeSigner tokenHeader accessClaimsSet
        refreshToken = encodeSigned encodeSigner tokenHeader refreshClaimsSet
    return $ IssueJWTResDTO accessToken refreshToken "csrfToken"