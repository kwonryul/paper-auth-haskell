{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Authentication(
    AuthenticationI(
        authContext
      )
) where

import qualified JWT.Repository
import JWT.Repository(JWTRepositoryI)
import qualified Role.Repository
import Role.Repository(RoleRepositoryI)

import JWT.Entity
import JWT.Model
import Role.Entity
import DB
import PaperMonad
import CallStack

import Servant
import Servant.Server.Experimental.Auth
import Network.Wai
import Database.Persist.Typed
import Web.JWT
import Web.Cookie
import Data.Aeson

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

class (JWTRepositoryI p, DBI p, RoleRepositoryI p) => AuthenticationI p where
    authContext :: HasCallStack => Proxy p -> PaperAuthPool -> VerifySigner -> AuthContext
    authContext = authContextImpl
    jwtAuthHandler :: HasCallStack => Proxy p -> PaperAuthPool -> VerifySigner -> AuthHandler Request AuthenticatedUser
    jwtAuthHandler = jwtAuthHandlerImpl
    jwtAuthHandler' :: HasCallStack => Proxy p -> PaperAuthPool -> VerifySigner -> Request -> Handler AuthenticatedUser
    jwtAuthHandler' = jwtAuthHandler'Impl
    jwtAuthHandler'' :: (HasCallStack, MonadUnliftIO m) => VerifySigner -> Request -> PaperAuthPool -> PaperMonad p m AuthenticatedUser
    jwtAuthHandler'' = jwtAuthHandler''Impl
    jwtAuthHandler''' :: (HasCallStack, MonadUnliftIO m) => VerifySigner -> Request -> PaperAuthConn -> PaperMonad p m AuthenticatedUser
    jwtAuthHandler''' = jwtAuthHandler'''Impl
    jwtAuthRefreshHandler :: HasCallStack => Proxy p -> PaperAuthPool -> VerifySigner -> AuthHandler Request AuthenticatedUserRefresh
    jwtAuthRefreshHandler = jwtAuthRefreshHandlerImpl
    jwtAuthRefreshHandler' :: HasCallStack => Proxy p -> PaperAuthPool -> VerifySigner -> Request -> Handler AuthenticatedUserRefresh
    jwtAuthRefreshHandler' = jwtAuthRefreshHandler'Impl
    jwtAuthRefreshHandler'' :: (HasCallStack, MonadUnliftIO m) =>  VerifySigner -> Request -> PaperAuthPool -> PaperMonad p m AuthenticatedUserRefresh
    jwtAuthRefreshHandler'' = jwtAuthRefreshHandler''Impl
    jwtAuthRefreshHandler''' :: (HasCallStack, MonadUnliftIO m) => VerifySigner -> Request -> PaperAuthConn -> PaperMonad p m AuthenticatedUserRefresh
    jwtAuthRefreshHandler''' = jwtAuthRefreshHandler'''Impl
    getAccessTokenAndRoleSet :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> VerifySigner -> Request -> UTCTime -> PaperMonad p m (AccessTokenId, AccessToken, Set Role)
    getAccessTokenAndRoleSet = getAccessTokenAndRoleSetImpl
    getRefreshToken :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> VerifySigner -> Request -> UTCTime -> PaperMonad p m (RefreshTokenId, RefreshToken)
    getRefreshToken = getRefreshTokenImpl

authContextImpl :: (HasCallStack, AuthenticationI p) => Proxy p -> PaperAuthPool -> VerifySigner -> AuthContext
authContextImpl p pool verifySigner = jwtAuthHandler p pool verifySigner :. jwtAuthRefreshHandler p pool verifySigner :. EmptyContext

jwtAuthHandlerImpl :: (HasCallStack, AuthenticationI p) => Proxy p -> PaperAuthPool -> VerifySigner -> AuthHandler Request AuthenticatedUser
jwtAuthHandlerImpl p pool verifySigner = mkAuthHandler $ jwtAuthHandler' p pool verifySigner

jwtAuthHandler'Impl :: forall p. (HasCallStack, AuthenticationI p) => Proxy p -> PaperAuthPool -> VerifySigner -> Request -> Handler AuthenticatedUser
jwtAuthHandler'Impl _ pool verifySigner request = runPaperMonad $ jwtAuthHandler'' @p verifySigner request pool

jwtAuthHandler''Impl :: (HasCallStack, AuthenticationI p, MonadUnliftIO m) => VerifySigner -> Request -> PaperAuthPool -> PaperMonad p m AuthenticatedUser
jwtAuthHandler''Impl verifySigner request pool = runSqlPoolOneConnection (jwtAuthHandler''' verifySigner request) pool

jwtAuthHandler'''Impl :: (HasCallStack, AuthenticationI p, MonadUnliftIO m) => VerifySigner -> Request -> PaperAuthConn -> PaperMonad p m AuthenticatedUser
jwtAuthHandler'''Impl verifySigner request conn = do
    currentUTC <- paperLiftIOUnliftIO getCurrentTime
    (accessTokenId, AccessToken { accessTokenUserId, accessTokenRefreshTokenId }, roleSet) <- getAccessTokenAndRoleSet conn verifySigner request currentUTC
    (refreshTokenId, RefreshToken { refreshTokenUserId }) <- getRefreshToken conn verifySigner request currentUTC
    paperAssert (accessTokenRefreshTokenId == refreshTokenId) $ PaperError "accessToken and refreshToken not match" (err401 { errBody = "accessToken and refreshToken not match" }) callStack'
    paperAssert (accessTokenUserId == refreshTokenUserId) $ PaperError "userId invalid" (err401 { errBody = "userId invalid" }) callStack'
    return $ AuthenticatedUser accessTokenId refreshTokenId accessTokenUserId roleSet

jwtAuthRefreshHandlerImpl :: (HasCallStack, AuthenticationI p) => Proxy p -> PaperAuthPool -> VerifySigner -> AuthHandler Request AuthenticatedUserRefresh
jwtAuthRefreshHandlerImpl p pool verifySigner = mkAuthHandler $ jwtAuthRefreshHandler' p pool verifySigner

jwtAuthRefreshHandler'Impl :: forall p. (HasCallStack, AuthenticationI p) => Proxy p -> PaperAuthPool -> VerifySigner -> Request -> Handler AuthenticatedUserRefresh
jwtAuthRefreshHandler'Impl _ pool verifySigner request = runPaperMonad $ jwtAuthRefreshHandler'' @p verifySigner request pool

jwtAuthRefreshHandler''Impl :: (HasCallStack, AuthenticationI p, MonadUnliftIO m) => VerifySigner -> Request -> PaperAuthPool -> PaperMonad p m AuthenticatedUserRefresh
jwtAuthRefreshHandler''Impl verifySigner request pool = runSqlPoolOneConnection (jwtAuthRefreshHandler''' verifySigner request) pool

jwtAuthRefreshHandler'''Impl :: (HasCallStack, AuthenticationI p, MonadUnliftIO m) => VerifySigner -> Request -> PaperAuthConn -> PaperMonad p m AuthenticatedUserRefresh
jwtAuthRefreshHandler'''Impl verifySigner request conn = do
    currentUTC <- paperLiftIOUnliftIO getCurrentTime
    (refreshTokenId, RefreshToken { refreshTokenUserId }) <- getRefreshToken conn verifySigner request currentUTC
    return $ AuthenticatedUserRefresh refreshTokenId refreshTokenUserId

getAccessTokenAndRoleSetImpl :: forall p m. (HasCallStack, AuthenticationI p, MonadUnliftIO m) => PaperAuthConn -> VerifySigner -> Request -> UTCTime -> PaperMonad p m (AccessTokenId, AccessToken, Set Role)
getAccessTokenAndRoleSetImpl conn verifySigner request currentUTC = do
    accessBearerJWT <- maybeToPaperMonad (Prelude.lookup "Authorization" $ requestHeaders request) $
        PaperError "missing accessToken" (err401 { errBody = "missing accessToken" }) callStack'
    accessJwt <- removeBearer accessBearerJWT
    accessVerifiedJWT <- maybeToPaperMonad (decodeAndVerifySignature verifySigner $ Data.Text.Encoding.decodeUtf8 accessJwt) $
        PaperError "accessToken verification failed" (err401 { errBody = "accessToken verification failed" }) callStack'
    let JWTClaimsSet {
        sub = accessSub, nbf = accessNbf, jti = accessJti, unregisteredClaims = accessUnregisteredClaims
        } = claims accessVerifiedJWT
    accessUserId' <- maybeToPaperMonad accessSub $ PaperError "missing accessSubject" (err401 { errBody = "missing accessSubject" }) callStack'
    let accessUserId = toSqlKeyFor $ read $ show accessUserId'
    accessTokenId' <- maybeToPaperMonad accessJti $ PaperError "missing accessJti" (err401 { errBody = "missing accessJti" }) callStack'
    let accessTokenId = toSqlKeyFor $ read $ show accessTokenId'
    roles <- maybeToPaperMonad
        (Data.Map.lookup "roles" $ unClaimsMap accessUnregisteredClaims) $
        PaperError "missing roles" (err401 { errBody = "missing roles" }) callStack'
    roleNameList <- case roles of
        Array vector -> do
            let valueList = Data.Vector.toList vector
            Data.Traversable.mapM (\case
                String name -> return $ Data.Text.unpack name
                _ -> toPaperMonad $ PaperError  "roles invalid" (err401 { errBody = "roles invalid" }) callStack'
                ) valueList
        _ -> toPaperMonad $ PaperError "roles invalid" (err401 { errBody = "roles invalid" }) callStack'
    case accessNbf of
        Just nbf -> do
            let nbfUTC = addUTCTime (secondsSinceEpoch nbf) $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
            if diffUTCTime currentUTC nbfUTC < 0 then
                toPaperMonad $ PaperError "not reached accessNbf" (err401 { errBody = "not reached accessNbf" }) callStack'
            else
                return ()
        Nothing -> return ()
    accessToken' <- JWT.Repository.findByAccessTokenId conn accessTokenId
    accessToken@AccessToken { accessTokenExpire } <- maybeToPaperMonad accessToken' $ PaperError "accessToken invalidated" (err401 { errBody = "accessToken invalidated" }) callStack'
    case accessTokenExpire of
        Just accessTokenExpire' ->
            if diffUTCTime currentUTC accessTokenExpire' > 0 then
                toPaperMonad $ PaperError "accessToken expired" (err401 { errBody = "accessToken expired" }) callStack'
            else
                return ()
        Nothing -> return ()
    paperAssert (accessTokenUserId accessToken == accessUserId) $
        PaperError "accessUserId invalid" (err401 { errBody = "accessUserId invalid" }) callStack'
    roleSet <- Role.Repository.getRoleSetByNameList roleNameList conn
    return (accessTokenId, accessToken, roleSet)
    where
        removeBearer :: (HasCallStack, MonadUnliftIO m) => ByteString -> PaperMonad p m ByteString
        removeBearer bs = do
            if Data.ByteString.Char8.isPrefixOf "Bearer " bs then
                return $ Data.ByteString.Char8.drop 7 bs
            else
                toPaperMonad $ PaperError "jwtToken should be Bearer" (err401 { errBody = "jwtToken should be Bearer" }) callStack'

getRefreshTokenImpl :: forall p m. (HasCallStack, AuthenticationI p, MonadUnliftIO m) => PaperAuthConn -> VerifySigner -> Request -> UTCTime -> PaperMonad p m (RefreshTokenId, RefreshToken)
getRefreshTokenImpl conn verifySigner request currentUTC = do
    cookie <- maybeToPaperMonad (Prelude.lookup "Cookie" $ requestHeaders request) $
        PaperError "missing refreshToken" (err401 { errBody = "missing refreshToken" }) callStack'
    refreshJWT <- maybeToPaperMonad (Prelude.lookup "Paper-Refresh-Token" $ parseCookies cookie) $
        PaperError "missing refreshToken" (err401 { errBody = "missing refreshToken" }) callStack'
    refreshVerifiedJWT <- maybeToPaperMonad (decodeAndVerifySignature verifySigner $ Data.Text.Encoding.decodeUtf8 refreshJWT) $
        PaperError "refreshToken verification failed" (err401 { errBody = "accessToken verification failed" }) callStack'
    let JWTClaimsSet {
        sub = refreshSub, nbf = refreshNbf, jti = refreshJti
        } = claims refreshVerifiedJWT
    refreshUserId' <- maybeToPaperMonad refreshSub $ PaperError "missing refreshSubject" (err401 { errBody = "missing refreshSubject" }) callStack'
    let refreshUserId = toSqlKeyFor $ read $ show refreshUserId'
    refreshTokenId' <- maybeToPaperMonad refreshJti $ PaperError "missing refreshJti" (err401 { errBody = "missing refreshJti" }) callStack'
    let refreshTokenId = toSqlKeyFor $ read $ show refreshTokenId'
    case refreshNbf of
        Just nbf -> do
            let nbfUTC = addUTCTime (secondsSinceEpoch nbf) $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
            if diffUTCTime currentUTC nbfUTC < 0 then
                toPaperMonad $ PaperError "not reached refreshNbf" (err401 { errBody = "not reached refreshNbf"}) callStack'
            else
                return ()
        Nothing -> return ()
    refreshToken' <- JWT.Repository.findByRefreshTokenId conn refreshTokenId
    refreshToken@RefreshToken { refreshTokenExpire } <- maybeToPaperMonad refreshToken' $ PaperError "refreshToken invalidated" (err401 { errBody = "refreshToken invalidated" }) callStack'
    case refreshTokenExpire of
        Just refreshTokenExpire' ->
            if diffUTCTime currentUTC refreshTokenExpire' > 0 then
                toPaperMonad $ PaperError "refreshToken expired" (err401 { errBody = "refreshToken expired" }) callStack'
            else
                return ()
        Nothing -> return ()
    paperAssert (refreshTokenUserId refreshToken == refreshUserId) $
        PaperError "refreshUserId invalid" (err401 { errBody = "refreshUserId invalid" }) callStack'
    return (refreshTokenId, refreshToken)