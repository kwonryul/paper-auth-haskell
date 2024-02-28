{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Authentication(
    authContext
) where

import qualified JWT.Repository
import qualified Role.Repository

import JWT.Entity
import JWT.Model
import Role.Entity
import DB
import PaperError
import CallStack

import Servant
import Servant.Server.Experimental.Auth
import Network.Wai
import Database.Persist.Sql
import Database.Persist.Typed
import Web.JWT
import Web.Cookie
import Data.Aeson

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Traversable
import Data.Map
import Data.Set
import Data.Vector
import Data.Time
import Data.ByteString.Char8
import Data.Text
import Data.Text.Encoding
import GHC.Stack

type AuthContext = Context '[AuthHandler Request AuthenticatedUser, AuthHandler Request AuthenticatedUserRefresh]

type instance AuthServerData (AuthProtect "jwt-auth") = AuthenticatedUser
type instance AuthServerData (AuthProtect "jwt-auth-refresh") = AuthenticatedUserRefresh

authContext :: HasCallStack => PaperAuthPool -> VerifySigner -> AuthContext
authContext pool verifySigner = jwtAuthHandler pool verifySigner :. jwtAuthRefreshHandler pool verifySigner :. EmptyContext

jwtAuthHandler :: HasCallStack => PaperAuthPool -> VerifySigner -> AuthHandler Request AuthenticatedUser
jwtAuthHandler pool verifySigner = mkAuthHandler $ jwtAuthHandler' pool verifySigner

jwtAuthHandler' :: HasCallStack => PaperAuthPool -> VerifySigner -> Request -> Handler AuthenticatedUser
jwtAuthHandler' pool verifySigner request = runPaperExceptT $ jwtAuthHandler'' pool verifySigner request

jwtAuthHandler'' :: (HasCallStack, MonadUnliftIO m) => PaperAuthPool -> VerifySigner -> Request -> PaperExceptT m AuthenticatedUser
jwtAuthHandler'' pool verifySigner request = do
    unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\conn ->
        catchE (inner conn) $ \e -> do
            paperLift $ runReaderT transactionUndo (generalizeSqlBackend conn)
            ExceptT $ return $ Left e
            )) pool
    where
        inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PaperExceptT m AuthenticatedUser
        inner conn = jwtAuthHandler''' conn verifySigner request

jwtAuthHandler''' :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> VerifySigner -> Request -> PaperExceptT m AuthenticatedUser
jwtAuthHandler''' conn verifySigner request = do
    currentUTC <- paperLiftIO getCurrentTime
    (accessTokenId, AccessToken { accessTokenUserId, accessTokenRefreshTokenId }, roleSet) <- getAccessTokenAndRoleSet conn verifySigner request currentUTC
    (refreshTokenId, RefreshToken { refreshTokenUserId }) <- getRefreshToken conn verifySigner request currentUTC
    paperAssert (accessTokenRefreshTokenId == refreshTokenId) $ PaperException "accessToken and refreshToken not match" (err401 { errBody = "accessToken and refreshToken not match" }) callStack'
    paperAssert (accessTokenUserId == refreshTokenUserId) $ PaperException "userId invalid" (err401 { errBody = "userId invalid" }) callStack'
    return $ AuthenticatedUser accessTokenId refreshTokenId accessTokenUserId roleSet

jwtAuthRefreshHandler :: HasCallStack => PaperAuthPool -> VerifySigner -> AuthHandler Request AuthenticatedUserRefresh
jwtAuthRefreshHandler pool verifySigner = mkAuthHandler $ jwtAuthRefreshHandler' pool verifySigner

jwtAuthRefreshHandler' :: HasCallStack => PaperAuthPool -> VerifySigner -> Request -> Handler AuthenticatedUserRefresh
jwtAuthRefreshHandler' pool verifySigner request = runPaperExceptT $ jwtAuthRefreshHandler'' pool verifySigner request

jwtAuthRefreshHandler'' :: (HasCallStack, MonadUnliftIO m) => PaperAuthPool -> VerifySigner -> Request -> PaperExceptT m AuthenticatedUserRefresh
jwtAuthRefreshHandler'' pool verifySigner request = do
    unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\conn ->
        catchE (inner conn) $ \e -> do
            paperLift $ runReaderT transactionUndo (generalizeSqlBackend conn)
            ExceptT $ return $ Left e
            )) pool
    where
        inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PaperExceptT m AuthenticatedUserRefresh
        inner conn = jwtAuthRefreshHandler''' conn verifySigner request

jwtAuthRefreshHandler''' :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> VerifySigner -> Request -> PaperExceptT m AuthenticatedUserRefresh
jwtAuthRefreshHandler''' conn verifySigner request = do
    currentUTC <- paperLiftIO getCurrentTime
    (refreshTokenId, RefreshToken { refreshTokenUserId }) <- getRefreshToken conn verifySigner request currentUTC
    return $ AuthenticatedUserRefresh refreshTokenId refreshTokenUserId

getAccessTokenAndRoleSet :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> VerifySigner -> Request -> UTCTime -> PaperExceptT m (AccessTokenId, AccessToken, Set Role)
getAccessTokenAndRoleSet conn verifySigner request currentUTC = do
    accessBearerJWT <- maybeToPaperExceptT (Prelude.lookup "Authorization" $ requestHeaders request) $
        PaperException "missing accessToken" (err401 { errBody = "missing accessToken" }) callStack'
    accessJwt <- removeBearer accessBearerJWT
    accessVerifiedJWT <- maybeToPaperExceptT (decodeAndVerifySignature verifySigner $ Data.Text.Encoding.decodeUtf8 accessJwt) $
        PaperException "accessToken verification failed" (err401 { errBody = "accessToken verification failed" }) callStack'
    let JWTClaimsSet {
        sub = accessSub, nbf = accessNbf, jti = accessJti, unregisteredClaims = accessUnregisteredClaims
        } = claims accessVerifiedJWT
    accessUserId' <- maybeToPaperExceptT accessSub $ PaperException "missing accessSubject" (err401 { errBody = "missing accessSubject" }) callStack'
    let accessUserId = toSqlKeyFor $ read $ show accessUserId'
    accessTokenId' <- maybeToPaperExceptT accessJti $ PaperException "missing accessJti" (err401 { errBody = "missing accessJti" }) callStack'
    let accessTokenId = toSqlKeyFor $ read $ show accessTokenId'
    roles <- maybeToPaperExceptT
        (Data.Map.lookup "roles" $ unClaimsMap accessUnregisteredClaims) $
        PaperException "missing roles" (err401 { errBody = "missing roles" }) callStack'
    roleNameList <- case roles of
        Array vector -> do
            let valueList = Data.Vector.toList vector
            Data.Traversable.mapM (\case
                String name -> return $ Data.Text.unpack name
                _ -> toPaperExceptT $ PaperException  "roles invalid" (err401 { errBody = "roles invalid" }) callStack'
                ) valueList
        _ -> toPaperExceptT $ PaperException "roles invalid" (err401 { errBody = "roles invalid" }) callStack'
    case accessNbf of
        Just nbf -> do
            let nbfUTC = addUTCTime (secondsSinceEpoch nbf) $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
            if diffUTCTime currentUTC nbfUTC < 0 then
                toPaperExceptT $ PaperException "not reached accessNbf" (err401 { errBody = "not reached accessNbf" }) callStack'
            else
                return ()
        Nothing -> return ()
    accessToken' <- JWT.Repository.findByAccessTokenId conn accessTokenId
    accessToken@AccessToken { accessTokenExpire } <- maybeToPaperExceptT accessToken' $ PaperException "accessToken invalidated" (err401 { errBody = "accessToken invalidated" }) callStack'
    case accessTokenExpire of
        Just accessTokenExpire' ->
            if diffUTCTime currentUTC accessTokenExpire' > 0 then
                toPaperExceptT $ PaperException "accessToken expired" (err401 { errBody = "accessToken expired" }) callStack'
            else
                return ()
        Nothing -> return ()
    paperAssert (accessTokenUserId accessToken == accessUserId) $
        PaperException "accessUserId invalid" (err401 { errBody = "accessUserId invalid" }) callStack'
    roleSet <- Role.Repository.getRoleSetByNameList conn roleNameList
    return (accessTokenId, accessToken, roleSet)
    where
        removeBearer :: (HasCallStack, MonadUnliftIO m) => ByteString -> PaperExceptT m ByteString
        removeBearer bs = do
            if Data.ByteString.Char8.isPrefixOf "Bearer " bs then
                return $ Data.ByteString.Char8.drop 7 bs
            else
                toPaperExceptT $ PaperException "jwtToken should be Bearer" (err401 { errBody = "jwtToken should be Bearer" }) callStack'

getRefreshToken :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> VerifySigner -> Request -> UTCTime -> PaperExceptT m (RefreshTokenId, RefreshToken)
getRefreshToken conn verifySigner request currentUTC = do
    cookie <- maybeToPaperExceptT (Prelude.lookup "Cookie" $ requestHeaders request) $
        PaperException "missing refreshToken" (err401 { errBody = "missing refreshToken" }) callStack'
    refreshJWT <- maybeToPaperExceptT (Prelude.lookup "Paper-Refresh-Token" $ parseCookies cookie) $
        PaperException "missing refreshToken" (err401 { errBody = "missing refreshToken" }) callStack'
    refreshVerifiedJWT <- maybeToPaperExceptT (decodeAndVerifySignature verifySigner $ Data.Text.Encoding.decodeUtf8 refreshJWT) $
        PaperException "refreshToken verification failed" (err401 { errBody = "accessToken verification failed" }) callStack'
    let JWTClaimsSet {
        sub = refreshSub, nbf = refreshNbf, jti = refreshJti
        } = claims refreshVerifiedJWT
    refreshUserId' <- maybeToPaperExceptT refreshSub $ PaperException "missing refreshSubject" (err401 { errBody = "missing refreshSubject" }) callStack'
    let refreshUserId = toSqlKeyFor $ read $ show refreshUserId'
    refreshTokenId' <- maybeToPaperExceptT refreshJti $ PaperException "missing refreshJti" (err401 { errBody = "missing refreshJti" }) callStack'
    let refreshTokenId = toSqlKeyFor $ read $ show refreshTokenId'
    case refreshNbf of
        Just nbf -> do
            let nbfUTC = addUTCTime (secondsSinceEpoch nbf) $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
            if diffUTCTime currentUTC nbfUTC < 0 then
                toPaperExceptT $ PaperException "not reached refreshNbf" (err401 { errBody = "not reached refreshNbf"}) callStack'
            else
                return ()
        Nothing -> return ()
    refreshToken' <- JWT.Repository.findByRefreshTokenId conn refreshTokenId
    refreshToken@RefreshToken { refreshTokenExpire } <- maybeToPaperExceptT refreshToken' $ PaperException "refreshToken invalidated" (err401 { errBody = "refreshToken invalidated" }) callStack'
    case refreshTokenExpire of
        Just refreshTokenExpire' ->
            if diffUTCTime currentUTC refreshTokenExpire' > 0 then
                toPaperExceptT $ PaperException "refreshToken expired" (err401 { errBody = "refreshToken expired" }) callStack'
            else
                return ()
        Nothing -> return ()
    paperAssert (refreshTokenUserId refreshToken == refreshUserId) $
        PaperException "refreshUserId invalid" (err401 { errBody = "refreshUserId invalid" }) callStack'
    return (refreshTokenId, refreshToken)