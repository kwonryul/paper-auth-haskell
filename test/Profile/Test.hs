{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Profile.Test() where

import JWT.Controller
import JWT.Repository
import JWT.Service
import JWT.Util
import Role.Repository
import User.Controller
import User.Repository
import User.Service
import Verification.DTO
import Verification.Repository
import Verification.Service
import Verification.Util
import Authentication
import Context
import DB
import GlobalMonad
import PaperApp
import Lib

import Paths_paper_auth
import Monad.ErrorT
import PaperMonad
import Monad.ProfileT
import JWT.Model

import Profile.Test.Import
import Profile.Test.Snippet

import Data.Configurator

import Database.Persist.Typed
import Database.Persist.MySQL

import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Logger
import Data.Set

instance JWTControllerI Test
instance JWTRepositoryI Test
instance JWTServiceI Test
instance JWTUtilI Test
instance RoleRepositoryI Test
instance UserControllerI Test
instance UserRepositoryI Test
instance UserServiceI Test
instance VerificationDTOI Test
instance VerificationRepositoryI Test
--instance VerificationServiceI Test
instance VerificationUtilI Test
--instance AuthenticationI Test
--instance ContextI Test
--instance DBI Test
instance LibI Test
--instance PaperAppI Test

instance ContextI Test where
    getConfig' = do
        filePath <- globalLiftIOUnliftIO $ getDataFileName "resources/application-test.cfg"
        globalLiftIOUnliftIO $ autoReload autoConfig [Required filePath]

instance PaperAppI Test where
    app p context docsFilePath staticFilePath = generateSnippetM context $ appImpl p context docsFilePath staticFilePath

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
        return $ AuthenticatedUser (toSqlKeyFor 64) (toSqlKeyFor 64) (toSqlKeyFor 64) Data.Set.empty
    jwtAuthRefreshHandler''' _ request conn = do
        return $ AuthenticatedUserRefresh (toSqlKeyFor 64) (toSqlKeyFor 64)

instance VerificationServiceI Test where
    issueJWT _ conn _ (PreAuthenticatedUser { userId }) currentUTC = do
        refreshJti <- JWT.Repository.newRefreshToken conn userId currentUTC Nothing
        accessJti <- JWT.Repository.newAccessToken conn userId currentUTC Nothing refreshJti
        let accessToken = "this is dummy access token"
            refreshToken = "this is dummy refresh token"
        JWT.Repository.saveAccessToken conn accessJti accessToken
        JWT.Repository.saveRefreshToken conn refreshJti refreshToken
        return $ JWTDTO accessJti accessToken refreshJti refreshToken