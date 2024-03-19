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
import Middleware.CORS
import Middleware.Utf8
import Monad.ErrorT
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
import User.Repository
import User.Service
import Verification.Controller
import Verification.ExDTO
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
import User.DTO
import Enum

import Profile.Test.Import

import Data.Configurator

import Servant
import Database.Persist.Typed
import Database.Persist.MySQL
import Network.Wai
import Web.Cookie
import Crypto.BCrypt

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Logger
import Data.Set
import Data.Time
import Data.Text
import Data.Text.Encoding
import Data.ByteString.Char8
import Text.Regex.TDFA
import System.Environment

instance JWTControllerI Test
--instance JWTExServiceI Test
instance JWTRepositoryI Test
instance JWTServiceI Test
--instance JWTUtilI Test
instance CORSI Test
instance Utf8I Test
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
instance UserRepositoryI Test
--instance UserServiceI Test
instance VerificationControllerI Test
instance VerificationExDTOI Test
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
        where
            profile :: Proxy Test
            profile = Proxy
    jwtAuthRefreshHandler''' _ request conn = do
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
        where
            profile :: Proxy Test
            profile = Proxy

instance JWTExServiceI Test where
    issueJWT _ _ (PreAuthenticatedUser { userId }) currentUTC conn = do
        refreshJti <- JWT.Repository.newRefreshToken userId currentUTC Nothing conn
        accessJti <- JWT.Repository.newAccessToken userId currentUTC Nothing refreshJti conn
        let accessToken = "this is dummy accessToken: " ++ show (fromSqlKeyFor userId)
            refreshToken = "this is dummy refreshToken: " ++ show (fromSqlKeyFor userId)
        JWT.Repository.saveAccessToken accessJti (Data.Text.pack accessToken) conn
        JWT.Repository.saveRefreshToken refreshJti (Data.Text.pack refreshToken) conn
        return $ JWTDTO accessJti (Data.Text.pack accessToken) refreshJti (Data.Text.pack refreshToken)

instance UserServiceI Test where
    enroll' config encodeSigner paperId password phoneNumber' _ _ conn = do
        profile <- ask
        phoneNumber <- stringToPhoneNumber phoneNumber'
        currentUTC <- paperLiftIOUnliftIO getCurrentTime
        sameUserIdEntityList <- User.Repository.findByPaperId paperId conn
        case sameUserIdEntityList of
            [] -> return ()
            _ ->
                toPaperMonad $ PaperError "paperId duplicate" (err400 { errBody = "paperId duplicate" }) (callStack' profile)
        samePhoneNumberList <- User.Repository.findByPhoneNumber phoneNumber conn
        case samePhoneNumberList of
            [] -> return ()
            _ ->
                toPaperMonad $ PaperError "phoneNumber duplicate" (err400 { errBody = "phoneNumber duplicate" }) (callStack' profile)
        hashedPassword <- maybeTToPaperMonadUnliftIO
            (MaybeT $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (Data.ByteString.Char8.pack password))
            $ PaperError "hashing string error" (err500 { errBody = "internal server error" }) (callStack' profile)
        userId <- User.Repository.newUser Paper (Just paperId) (Just hashedPassword) (Just phoneNumber) Nothing currentUTC conn
        let roleSet = Data.Set.empty
            preAuthenticatedUser = PreAuthenticatedUser { userId, roleSet }
        JWTDTO { accessToken, refreshToken } <- JWT.ExService.issueJWT config encodeSigner preAuthenticatedUser currentUTC conn
        let cookie = generateRefreshTokenCookie (Proxy :: Proxy Test) refreshToken
        return $ addHeader cookie $ EnrollResDTO { accessToken }

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
    getIdentifier _ _ _ = return "testIdentifier"

instance OAuth2ClientNaverExServiceI Test where
    getIdentifier _ _ _ = return "testIdentifier"