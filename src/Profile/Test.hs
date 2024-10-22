{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Profile.Test() where

import JWT.Controller
import JWT.ExService
import JWT.Repository
import JWT.Service
import JWT.Util
import Lock.Repository
import Middleware.CORS
import Middleware.Utf8
import Monad.ErrorT
import OAuth2.Client.GRpc.Controller
import OAuth2.Client.GRpc.ExService
import OAuth2.Client.ThirdParties.Kakao.ExService
import OAuth2.Client.ThirdParties.Naver.ExService
import OAuth2.Client.Controller
import OAuth2.Client.ExService
import OAuth2.Client.Repository
import OAuth2.Client.Service
import OAuth2.Client.Util
import Role.Repository
import SMS.Profile
import User.Controller
import User.ExService
import User.Repository
import User.Service
import Verification.Controller
import Verification.ExDTO
import Verification.ExService
import Verification.Repository
import Verification.Service
import Verification.Util
import Authentication
import CallStack
import Context
import DB
import GlobalMonad
import Lib
import PaperMonad
import Util

import SMS.Profile.None

import JWT.Model
import Monad.ProfileT

import Profile.Test.Import

import Data.Configurator

import Servant
import Database.Persist.Typed
import Database.Persist.MySQL
import Network.Wai
import Web.Cookie

import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Logger
import Data.Time
import Data.Text
import Data.Text.Encoding
import Text.Regex.TDFA
import System.Environment

instance JWTControllerI Test
--instance JWTExServiceI Test
instance JWTRepositoryI Test
instance JWTServiceI Test
--instance JWTUtilI Test
instance LockRepositoryI Test
instance CORSI Test
instance Utf8I Test
instance OAuth2ClientGRpcControllerI Test
instance OAuth2ClientGRpcExServiceI Test
--instance OAuth2ClientKakaoExServiceI Test
--instance OAuth2ClientNaverExServiceI Test
instance OAuth2ClientControllerI Test
instance OAuth2ClientExServiceI Test
instance OAuth2ClientRepositoryI Test
instance OAuth2ClientServiceI Test
instance OAuth2ClientUtilI Test
instance RoleRepositoryI Test
instance UserControllerI Test
instance UserExServiceI Test
instance UserRepositoryI Test
instance UserServiceI Test
instance VerificationControllerI Test
instance VerificationExDTOI Test
--instance VerificationExServiceI Test
instance VerificationRepositoryI Test
instance VerificationServiceI Test
instance VerificationUtilI Test
--instance AuthenticationI Test
--instance ContextI Test
--instance DBI Test
instance LibI Test
instance PaperAppI Test
instance UtilI Test

instance ContextI Test where
    getConfig' = do
        homeDir <- globalLiftIOUnliftIO $ getEnv "HOME"
        projectDir <- globalLiftIOUnliftIO $ Prelude.readFile $ homeDir ++ "/.paper-auth/project-directory"
        let filePath = projectDir ++ "resources/application-test.cfg"
        globalLiftIOUnliftIO $ autoReload autoConfig [Required filePath]

instance DBI Test where
    runSqlPoolOneConnection inner pool = do
        profile <- ask
        safeErrorTToPaperMonad $ unsafeToSafeUnliftIO $ ErrorT $ LoggingT $ (\logger ->
            ExceptT $ runSqlPoolFor (ReaderT (\conn -> do
                rt <- runExceptT $ (runLoggingT $ unErrorT $ unSafeErrorT $ runReaderT (unProfileT $ unPaperMonad $ inner conn) profile) logger
                runReaderT transactionUndo $ generalizeSqlBackend conn
                return rt
            )) pool
            )
    runSqlPoolOneConnectionGlobal inner pool = do
        profile <- ask
        safeErrorTToGlobalMonad $ unsafeToSafeUnliftIO $ ErrorT $ LoggingT $ (\logger ->
            ExceptT $ runSqlPoolFor (ReaderT (\conn -> do
                rt <- runExceptT $ (runLoggingT $ unErrorT $ unSafeErrorT $ runReaderT (unProfileT $ unGlobalMonad $ inner conn) profile) logger
                runReaderT transactionUndo $ generalizeSqlBackend conn
                return rt
            )) pool
            )

instance AuthenticationI Test where
    jwtAuthHandler''' _ request conn = do
        profile <- ask
        currentUTC <- paperLiftIOUnliftIO $ getCurrentTime
        accessToken <- maybeToPaperMonad (Prelude.lookup "Authorization" $ requestHeaders request) $
            PaperError "missing accessToken" (err401 { errBody = "missing accessToken" }) (callStack' profile)
        cookie <- maybeToPaperMonad (Prelude.lookup "Cookie" $ requestHeaders request) $
            PaperError "missing refreshToken" (err401 { errBody = "missing refreshToken" }) (callStack' profile)
        refreshToken <- maybeToPaperMonad (Prelude.lookup "Paper-Refresh-Token" $ parseCookies cookie) $
            PaperError "missing refreshToken" (err401 { errBody = "missing refreshToken" }) (callStack' profile)
        let userId = toSqlKeyFor $ read $ Data.Text.unpack $ Data.Text.Encoding.decodeUtf8 accessToken =~ ("([0-9])+" :: String)
            userId' = toSqlKeyFor $ read $ Data.Text.unpack $  Data.Text.Encoding.decodeUtf8 refreshToken =~ ("([0-9])+" :: String)
        PreAuthenticatedUser _ roleSet <- getPreAuthenticatedUser userId conn
        paperAssert (userId == userId') $ PaperError "userId invalid" (err401 { errBody = "userId invalid" }) (callStack' profile)
        refreshTokenId <- JWT.Repository.newRefreshToken userId currentUTC Nothing conn
        accessTokenId <- JWT.Repository.newAccessToken userId currentUTC Nothing refreshTokenId conn
        JWT.Repository.saveAccessToken accessTokenId (Data.Text.Encoding.decodeUtf8 accessToken) conn
        JWT.Repository.saveRefreshToken refreshTokenId (Data.Text.Encoding.decodeUtf8 refreshToken) conn
        return $ AuthenticatedUser accessTokenId refreshTokenId userId roleSet
    jwtAuthRefreshHandler''' _ request conn = do
        profile <- ask
        currentUTC <- paperLiftIOUnliftIO $ getCurrentTime
        cookie <- maybeToPaperMonad (Prelude.lookup "Cookie" $ requestHeaders request) $
            PaperError "missing refreshToken" (err401 { errBody = "missing refreshToken" }) (callStack' profile)
        refreshToken <- maybeToPaperMonad (Prelude.lookup "Paper-Refresh-Token" $ parseCookies cookie) $
            PaperError "missing refreshToken" (err401 { errBody = "missing refreshToken" }) (callStack' profile)
        let userId = toSqlKeyFor $ read $ Data.Text.unpack $  Data.Text.Encoding.decodeUtf8 refreshToken =~ ("([0-9])+" :: String)
        user' <- paperLiftUnliftIO $ runReaderT (get userId) conn
        case user' of
            Nothing -> toPaperMonad $ PaperError "user not found" (err401 { errBody = "user not found" }) (callStack' profile)
            _ -> return ()
        refreshTokenId <- JWT.Repository.newRefreshToken userId currentUTC Nothing conn
        JWT.Repository.saveRefreshToken refreshTokenId (Data.Text.Encoding.decodeUtf8 refreshToken) conn
        return $ AuthenticatedUserRefresh refreshTokenId userId

instance VerificationExServiceI Test where
    verifyVerification _ _ _ _ = return ()

instance JWTExServiceI Test where
    issueJWT _ _ (PreAuthenticatedUser { userId }) currentUTC conn = do
        refreshJti <- JWT.Repository.newRefreshToken userId currentUTC Nothing conn
        accessJti <- JWT.Repository.newAccessToken userId currentUTC Nothing refreshJti conn
        let accessToken = "this is dummy accessToken: " ++ show (fromSqlKeyFor userId)
            refreshToken = "this is dummy refreshToken: " ++ show (fromSqlKeyFor userId)
        JWT.Repository.saveAccessToken accessJti (Data.Text.pack accessToken) conn
        JWT.Repository.saveRefreshToken refreshJti (Data.Text.pack refreshToken) conn
        return $ JWTDTO accessJti (Data.Text.pack accessToken) refreshJti (Data.Text.pack refreshToken)
instance JWTUtilI Test where
    generateRefreshTokenCookie _ refreshToken =
        defaultSetCookie {
            setCookieName = "Paper-Refresh-Token"
          , setCookieValue = Data.Text.Encoding.encodeUtf8 refreshToken
          , setCookiePath = Just "/"
          , setCookieHttpOnly = False
          , setCookieSecure = False
          , setCookieSameSite = Nothing
          }

instance SMSProfileC Test where
    type SMSProfileF Test = SMSNone

instance OAuth2ClientKakaoExServiceI Test where
    getIdentifier _ _ = return "testIdentifier"

instance OAuth2ClientNaverExServiceI Test where
    getIdentifier _ _ _ = return "testIdentifier"