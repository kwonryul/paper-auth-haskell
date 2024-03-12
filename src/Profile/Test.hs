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
import Monad.ErrorT
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
import CORS
import DB
import GlobalMonad
import Lib
import PaperMonad
import Util

import SMS.Profile.None

import Import
import Monad.ProfileT
import JWT.Model
import User.DTO

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
instance CORSI Test
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
        PreAuthenticatedUser _ roleSet <- getPreAuthenticatedUser conn userId
        paperAssert (userId == userId') $ PaperError "userId invalid" (err401 { errBody = "userId invalid" }) (callStack' profile)
        refreshTokenId <- JWT.Repository.newRefreshToken conn userId currentUTC Nothing
        accessTokenId <- JWT.Repository.newAccessToken conn userId currentUTC Nothing refreshTokenId
        JWT.Repository.saveAccessToken conn accessTokenId (Data.Text.Encoding.decodeUtf8 accessToken)
        JWT.Repository.saveRefreshToken conn refreshTokenId (Data.Text.Encoding.decodeUtf8 refreshToken)
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
        refreshTokenId <- JWT.Repository.newRefreshToken conn userId currentUTC Nothing
        JWT.Repository.saveRefreshToken conn refreshTokenId (Data.Text.Encoding.decodeUtf8 refreshToken)
        return $ AuthenticatedUserRefresh refreshTokenId userId
        where
            profile :: Proxy Test
            profile = Proxy

instance JWTExServiceI Test where
    issueJWT _ conn _ (PreAuthenticatedUser { userId }) currentUTC = do
        refreshJti <- JWT.Repository.newRefreshToken conn userId currentUTC Nothing
        accessJti <- JWT.Repository.newAccessToken conn userId currentUTC Nothing refreshJti
        let accessToken = "this is dummy accessToken: " ++ show (fromSqlKeyFor userId)
            refreshToken = "this is dummy refreshToken: " ++ show (fromSqlKeyFor userId)
        JWT.Repository.saveAccessToken conn accessJti $ Data.Text.pack accessToken
        JWT.Repository.saveRefreshToken conn refreshJti $ Data.Text.pack refreshToken
        return $ JWTDTO accessJti (Data.Text.pack accessToken) refreshJti (Data.Text.pack refreshToken)

instance UserServiceI Test where
    enroll' config encodeSigner paperId password name phoneNumber' _ _ conn = do
        profile <- ask
        phoneNumber <- stringToPhoneNumber phoneNumber'
        currentUTC <- paperLiftIOUnliftIO getCurrentTime
        sameUserIdEntity' <- User.Repository.findByPaperId paperId conn
        case sameUserIdEntity' of
            Just _ ->
                toPaperMonad $ PaperError "paperId duplicate" (err400 { errBody = "paperId duplicate" }) (callStack' profile)
            Nothing -> return ()
        samePhoneNumberList <- User.Repository.findByPhoneNumber phoneNumber conn
        case samePhoneNumberList of
            [] -> return ()
            _ ->
                toPaperMonad $ PaperError "phoneNumber duplicate" (err400 { errBody = "phoneNumber duplicate" }) (callStack' profile)
        hashedPassword <- maybeTToPaperMonadUnliftIO
            (MaybeT $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (Data.ByteString.Char8.pack password))
            $ PaperError "hashing string error" (err500 { errBody = "Internal server error" }) (callStack' profile)
        userId <- User.Repository.newUser Paper paperId hashedPassword name (Just phoneNumber) currentUTC conn
        let roleSet = Data.Set.empty
            preAuthenticatedUser = PreAuthenticatedUser { userId, roleSet }
        JWTDTO { accessToken, refreshToken } <- JWT.ExService.issueJWT config conn encodeSigner preAuthenticatedUser currentUTC
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