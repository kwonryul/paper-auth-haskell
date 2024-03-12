{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Profile.Prod(
    Prod
) where

import Monad.ProfileT

import JWT.Controller
import JWT.Repository
import JWT.Service
import JWT.Util
import Monad.ErrorT
import Role.Repository
import SMS.Profile
import User.Controller
import User.Repository
import User.Service
import Verification.DTO
import Verification.Repository
import Verification.Service
import Verification.Util
import Authentication
import CallStack
import Configurator
import Context
import CORS
import DB
import GlobalMonad
import Lib
import PaperMonad
import Util

import SMS.Profile.NaverCloud

import ThirdParties.NaverCloud.Service

import Control.Monad.Logger
import Data.Text
import Data.Text.IO
import Data.Text.Encoding
import Data.ByteString
import Data.Time
import System.Directory

data Prod
instance Profile Prod

instance JWTControllerI Prod
instance JWTRepositoryI Prod
instance JWTServiceI Prod
instance JWTUtilI Prod
instance ErrorTI Prod
instance RoleRepositoryI Prod
instance UserControllerI Prod
instance UserRepositoryI Prod
instance UserServiceI Prod
instance VerificationDTOI Prod
instance VerificationRepositoryI Prod
instance VerificationServiceI Prod
instance VerificationUtilI Prod
instance AuthenticationI Prod
instance CallStackI Prod
instance ConfiguratorI Prod
instance ContextI Prod
instance CORSI Prod
instance DBI Prod
instance GlobalMonadI Prod
instance LibI Prod
instance PaperAppI Prod
instance PaperMonadI Prod
instance UtilI Prod

instance ErrorTProfile Prod PaperErrorP where
    defaultError _ _ = PaperDefaultError
    defaultLogger _ _ context = (\_ _ logLevel logStr -> do
        currentTime <- getCurrentTime
        let cfg = config context
            formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S\n" currentTime
        logDir :: String <- runGlobalMonadWithoutLog $ lookupRequiredGlobal @Prod cfg "log.paper"
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


instance ErrorTProfile Prod GlobalErrorP where
    defaultError _ _= GlobalDefaultError
    defaultLogger _ _ context = (\_ _ logLevel logStr -> do
        currentTime <- getCurrentTime
        let cfg = config context
            formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S\n" currentTime
        logDir :: String <- runGlobalMonadWithoutLog $ lookupRequiredGlobal @Prod cfg "log.global"
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

instance SMSProfileC Prod where
    type SMSProfileF Prod = SMSNaverCloud

instance NaverCloudServiceI Prod