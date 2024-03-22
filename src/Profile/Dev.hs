{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Profile.Dev(
    Dev
) where

import Monad.ProfileT

import JWT.Controller
import JWT.ExService
import JWT.Repository
import JWT.Service
import JWT.Util
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
import Configurator
import Context
import DB
import GlobalMonad
import Lib
import NestedMonad
import PaperMonad
import Util

import SMS.Profile.NaverCloud

import ThirdParties.NaverCloud.ExService

import Data.Configurator

import Control.Monad.Logger
import Data.Text
import Data.Text.IO
import Data.Text.Encoding
import Data.ByteString
import Data.Time
import System.Directory
import System.Environment

data Dev
instance Profile Dev

instance JWTControllerI Dev
instance JWTExServiceI Dev
instance JWTRepositoryI Dev
instance JWTServiceI Dev
instance JWTUtilI Dev
instance CORSI Dev
instance Utf8I Dev
instance ErrorTI Dev
instance OAuth2ClientGRpcControllerI Dev
instance OAuth2ClientGRpcExServiceI Dev
instance OAuth2ClientKakaoExServiceI Dev
instance OAuth2ClientNaverExServiceI Dev
instance OAuth2ClientControllerI Dev
instance OAuth2ClientExServiceI Dev
instance OAuth2ClientRepositoryI Dev
instance OAuth2ClientServiceI Dev
instance OAuth2ClientUtilI Dev
instance RoleRepositoryI Dev
instance UserControllerI Dev
instance UserExServiceI Dev
instance UserRepositoryI Dev
instance UserServiceI Dev
instance VerificationControllerI Dev
instance VerificationExDTOI Dev
instance VerificationExServiceI Dev
instance VerificationRepositoryI Dev
instance VerificationServiceI Dev
instance VerificationUtilI Dev
instance AuthenticationI Dev
instance CallStackI Dev
instance ConfiguratorI Dev
--instance ContextI Dev
instance DBI Dev
instance GlobalMonadI Dev
instance LibI Dev
instance NestedMonadI Dev
instance PaperAppI Dev
instance PaperMonadI Dev
instance UtilI Dev

instance ContextI Dev where
    getConfig' = do
        homeDir <- globalLiftIOUnliftIO $ getEnv "HOME"
        projectDir <- globalLiftIOUnliftIO $ Prelude.readFile $ homeDir ++ "/.paper-auth/project-directory"
        let filePath = projectDir ++ "resources/application-dev.cfg"
        globalLiftIOUnliftIO $ autoReload autoConfig [Required filePath]

instance ErrorTProfile Dev PaperErrorP where
    toOuterError _ _ (PaperInnerError { paperInnerServerError }) = paperInnerServerError
    defaultError _ _ = PaperDefaultError
    defaultLogger _ _ cfg = (\_ _ logLevel logStr -> do
        currentTime <- getCurrentTime
        let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S\n" currentTime
        logDir :: String <- runGlobalMonadWithoutLog $ lookupRequiredGlobal @Dev cfg "log"
        let (fileNameList, header) =
                case logLevel of
                    LevelDebug -> (["debug.log", "all.log"], "[DEBUG]\t")
                    LevelInfo -> (["info.log", "info-error.log", "info-warn-error.log", "all.log"], "[INFO]\t")
                    LevelWarn -> (["warn.log", "info-warn-error.log", "all.log"], "[WARN]\t")
                    LevelError -> (["error.log", "info-error.log", "info-warn-error.log", "all.log"], "[ERROR]\t")
                    LevelOther _ -> (["other.log", "all.log"], "[OTHER]\t")
            dirName = logDir ++ "paper/"
            filePathList = (\fileName -> dirName ++ fileName) <$> fileNameList
            content = Data.Text.Encoding.decodeUtf8 $ Data.ByteString.concat
                [
                    Data.Text.Encoding.encodeUtf8 $ Data.Text.pack (header ++ formattedDate)
                  , fromLogStr logStr
                  , Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "\n"
                  ]
        createDirectoryIfMissing True dirName
        mapM_ (\filePath ->
            Data.Text.IO.appendFile filePath content) filePathList
        )
    defaultErrorLog _ _ ie =
        (defaultLoc, "PaperErrorP", LevelError, toLogStr $ show ie)


instance ErrorTProfile Dev GlobalErrorP where
    toOuterError _ _ _ = userError $ "[Dev] global error"
    defaultError _ _= GlobalDefaultError
    defaultLogger _ _ cfg = (\_ _ logLevel logStr -> do
        currentTime <- getCurrentTime
        let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S\n" currentTime
        logDir :: String <- runGlobalMonadWithoutLog $ lookupRequiredGlobal @Dev cfg "log"
        let (fileNameList, header) =
                case logLevel of
                    LevelDebug -> (["debug.log", "all.log"], "[DEBUG]\t")
                    LevelInfo -> (["info.log", "info-error.log", "info-warn-error.log", "all.log"], "[INFO]\t")
                    LevelWarn -> (["warn.log", "info-warn-error.log", "all.log"], "[WARN]\t")
                    LevelError -> (["error.log", "info-error.log", "info-warn-error.log", "all.log"], "[ERROR]\t")
                    LevelOther _ -> (["other.log", "all.log"], "[OTHER]\t")
            dirName = logDir ++ "global/"
            filePathList = (\fileName -> dirName ++ fileName) <$> fileNameList
            content = Data.Text.Encoding.decodeUtf8 $ Data.ByteString.concat
                [
                    Data.Text.Encoding.encodeUtf8 $ Data.Text.pack (header ++ formattedDate)
                  , fromLogStr logStr
                  , Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "\n"
                  ]
        createDirectoryIfMissing True dirName
        mapM_ (\filePath ->
            Data.Text.IO.appendFile filePath content) filePathList
        )
    defaultErrorLog _ _ ie =
        (defaultLoc, "GlobalErrorP", LevelError, toLogStr $ show ie)

instance ErrorTProfile Dev NestedErrorP where
    toOuterError _ _ _ = userError $ "[Dev] nested error"
    defaultError _ _= NestedDefaultError
    defaultLogger _ _ cfg = (\_ _ logLevel logStr -> do
        currentTime <- getCurrentTime
        let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S\n" currentTime
        logDir :: String <- runNestedMonadWithoutLog $ lookupRequiredNested @Dev cfg "log"
        let (fileNameList, header) =
                case logLevel of
                    LevelDebug -> (["debug.log", "all.log"], "[DEBUG]\t")
                    LevelInfo -> (["info.log", "info-error.log", "info-warn-error.log", "all.log"], "[INFO]\t")
                    LevelWarn -> (["warn.log", "info-warn-error.log", "all.log"], "[WARN]\t")
                    LevelError -> (["error.log", "info-error.log", "info-warn-error.log", "all.log"], "[ERROR]\t")
                    LevelOther _ -> (["other.log", "all.log"], "[OTHER]\t")
            dirName = logDir ++ "nested/"
            filePathList = (\fileName -> dirName ++ fileName) <$> fileNameList
            content = Data.Text.Encoding.decodeUtf8 $ Data.ByteString.concat
                [
                    Data.Text.Encoding.encodeUtf8 $ Data.Text.pack (header ++ formattedDate)
                  , fromLogStr logStr
                  , Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "\n"
                  ]
        createDirectoryIfMissing True dirName
        mapM_ (\filePath ->
            Data.Text.IO.appendFile filePath content) filePathList
        )
    defaultErrorLog _ _ ie =
        (defaultLoc, "NestedErrorP", LevelError, toLogStr $ show ie)

instance SMSProfileC Dev where
    type SMSProfileF Dev = SMSNaverCloud

instance NaverCloudExServiceI Dev