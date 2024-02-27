{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Authentication(
    authContext
) where

import qualified JWT.Repository
import qualified Role.Repository

import JWT.Entity
import JWT.Model
import User.Entity
import DB
import PaperError
import CallStack

import Servant
import Servant.Server.Experimental.Auth
import Network.Wai
import Database.Persist.Sql
import Database.Persist.Typed
import Web.JWT
import Data.Aeson

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Traversable
import Data.Map
import Data.Vector
import Data.Time
import Data.ByteString.Char8
import Data.Text
import Data.Text.Encoding
import GHC.Stack

type AuthContext = Context '[AuthHandler Request AuthenticatedUser, AuthHandler Request AuthenticatedUserCsrf]

type instance AuthServerData (AuthProtect "jwt-auth") = AuthenticatedUser
type instance AuthServerData (AuthProtect "jwt-auth-csrf") = AuthenticatedUserCsrf

authContext :: HasCallStack => PaperAuthPool -> VerifySigner -> AuthContext
authContext pool verifySigner = jwtAuthHandler pool verifySigner :. jwtAuthCsrfHandler pool verifySigner :. EmptyContext

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
    (AccessToken { accessTokenExpire }, userId, roleNameList) <- getAccessTokenETC conn verifySigner request currentUTC
    getAuthenticatedUser conn currentUTC userId accessTokenExpire roleNameList

jwtAuthCsrfHandler :: HasCallStack => PaperAuthPool -> VerifySigner -> AuthHandler Request AuthenticatedUserCsrf
jwtAuthCsrfHandler pool verifySigner = mkAuthHandler $ jwtAuthCsrfHandler' pool verifySigner

jwtAuthCsrfHandler' :: HasCallStack => PaperAuthPool -> VerifySigner -> Request -> Handler AuthenticatedUserCsrf
jwtAuthCsrfHandler' pool verifySigner request = runPaperExceptT $ jwtAuthCsrfHandler'' pool verifySigner request

jwtAuthCsrfHandler'' :: (HasCallStack, MonadUnliftIO m) => PaperAuthPool -> VerifySigner -> Request -> PaperExceptT m AuthenticatedUserCsrf
jwtAuthCsrfHandler'' pool verifySigner request = do
    unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\conn ->
        catchE (inner conn) $ \e -> do
            paperLift $ runReaderT transactionUndo (generalizeSqlBackend conn)
            ExceptT $ return $ Left e
            )) pool
        where
            inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PaperExceptT m AuthenticatedUserCsrf
            inner conn = jwtAuthCsrfHandler''' conn verifySigner request

jwtAuthCsrfHandler''' :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> VerifySigner -> Request -> PaperExceptT m AuthenticatedUserCsrf
jwtAuthCsrfHandler''' conn verifySigner request = do
    currentUTC <- paperLiftIO getCurrentTime
    (AccessToken {
        accessTokenExpire
      , accessTokenCsrfToken
      }, userId, roleNameList) <- getAccessTokenETC conn verifySigner request currentUTC
    authenticatedUserCsrf <- AuthenticatedUserCsrf <$> getAuthenticatedUser conn currentUTC userId accessTokenExpire roleNameList
    csrfToken' <- maybeToPaperExceptT (Prelude.lookup "X-CSRF-Token" $ requestHeaders request) $
        PaperException "missing csrfToken" (err401 { errBody = "missing csrfToken" }) callStack'
    paperAssert (csrfToken' == Data.Text.Encoding.encodeUtf8 accessTokenCsrfToken) $
        PaperException "csrfToken invalid" (err401 { errBody = "csrfToken invalid" }) callStack'
    return authenticatedUserCsrf

getAccessTokenETC :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> VerifySigner -> Request -> UTCTime -> PaperExceptT m (AccessToken, UserId, [String])
getAccessTokenETC conn verifySigner request currentUTC = do
    bearerJWT <- maybeToPaperExceptT (Prelude.lookup "Authorization" $ requestHeaders request) $
        PaperException "missing jwtToken" (err401 { errBody = "missing jwtToken" }) callStack'
    jwt <- removeBearer bearerJWT
    verifiedJWT <- maybeToPaperExceptT (decodeAndVerifySignature verifySigner $ Data.Text.Encoding.decodeUtf8 jwt) $
        PaperException "jwtToken verification failed" (err401 { errBody = "jwtToken verification failed" }) callStack'
    let JWTClaimsSet { sub, nbf, jti, unregisteredClaims } = claims verifiedJWT
    userId' <- maybeToPaperExceptT sub $ PaperException "missing subject" (err401 { errBody = "missing subject" }) callStack'
    let userId = toSqlKeyFor $ read $ show userId'
    accessTokenId' <- maybeToPaperExceptT jti $ PaperException "missing jti" (err401 { errBody = "missing jti" }) callStack'
    let accessTokenId = toSqlKeyFor $ read $ show  accessTokenId'
    roles <- maybeToPaperExceptT
        (Data.Map.lookup "roles" $ unClaimsMap unregisteredClaims) $
        PaperException "missing roles" (err401 { errBody = "missing roles" }) callStack'
    roleNameList <- case roles of
            Array vector -> do
                let valueList = Data.Vector.toList vector
                Data.Traversable.mapM (\case
                    String name -> return $ Data.Text.unpack name
                    _ -> toPaperExceptT $ PaperException "roles invalid" (err401 { errBody = "roles invalid" }) callStack'
                    ) valueList
            _ -> toPaperExceptT $ PaperException "roles invalid" (err401 { errBody = "roles invalid" }) callStack'
    case nbf of
        Just nbf' -> do
            let nbfUTC = addUTCTime (secondsSinceEpoch nbf') $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
            if diffUTCTime currentUTC nbfUTC < 0 then
                toPaperExceptT $ PaperException "not reached nbf" (err401 { errBody = "not reached nbf" }) callStack'
            else
                return ()
        Nothing -> return ()
    accessToken' <- JWT.Repository.findByAccessTokenId conn accessTokenId
    accessToken <- maybeToPaperExceptT accessToken' $ PaperException "accessToken invalidated" (err401 { errBody = "accessToken invalidated" }) callStack'
    return (accessToken, userId, roleNameList)
    where
        removeBearer :: (HasCallStack, MonadUnliftIO m) => ByteString -> PaperExceptT m ByteString
        removeBearer bs = do
            if Data.ByteString.Char8.isPrefixOf "Bearer " bs then
                return $ Data.ByteString.Char8.drop 7 bs
            else
                toPaperExceptT $ PaperException "jwtToken should be Bearer" (err401 { errBody = "jwtToken should be Bearer" }) callStack'

getAuthenticatedUser :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> UTCTime -> UserId -> Maybe UTCTime -> [String] -> PaperExceptT m AuthenticatedUser
getAuthenticatedUser conn currentUTC userId accessTokenExpire roleNameList = do
    case accessTokenExpire of
        Just accessTokenExpire' ->
            if diffUTCTime currentUTC accessTokenExpire' > 0 then
                toPaperExceptT $ PaperException "accessToken expired" (err401 { errBody = "accessToken expired" }) callStack'
            else
                return ()
        Nothing -> return ()
    roleSet <- Role.Repository.getRoleSetByNameList conn roleNameList
    return $ AuthenticatedUser { userId, roleSet }