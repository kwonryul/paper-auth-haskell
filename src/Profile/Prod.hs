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
import DB
import GlobalMonad
import Lib
import PaperApp
import PaperMonad

import Data.Text
import Data.Text.IO
import Data.Text.Encoding
import Data.ByteString
import Control.Monad.Logger
import Data.Time

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
instance DBI Prod
instance GlobalMonadI Prod
instance LibI Prod
instance PaperAppI Prod
instance PaperMonadI Prod

instance ErrorTProfile Prod PaperErrorP where
    defaultError _ _ = PaperDefaultError
    defaultLogger _ _ context = (\_ _ logLevel logStr -> do
        let cfg = config context
        logDir :: String <- runGlobalMonadWithoutLog $ lookupRequiredGlobal @Prod cfg "log.paper"
        let (fileNameList, header) =
                case logLevel of
                    LevelDebug -> (["debug.log", "all.log"], "[DEBUG]\n")
                    LevelInfo -> (["info.log", "info-error.log", "info-warn-error.log", "all.log"], "[INFO]\n")
                    LevelWarn -> (["warn.log", "info-warn-error.log", "all.log"], "[WARN]\n")
                    LevelError -> (["error.log", "info-error.log", "info-warn-error.log", "all.log"], "[ERROR]\n")
                    LevelOther _ -> (["other.log", "all.log"], "[OTHER]\n")
            filePathList = (\fileName -> logDir ++ "/paper/" ++ fileName) <$> fileNameList
            content = Data.Text.Encoding.decodeUtf8 $ Data.ByteString.concat
                [Data.Text.Encoding.encodeUtf8 $ Data.Text.pack header, fromLogStr logStr]
        mapM_ (\filePath ->
            Data.Text.IO.appendFile filePath content) filePathList
        )
    defaultErrorLog _ _ ie currentTime =
        (defaultLoc, "PaperErrorP", LevelError, paperLogStr)
        where
            paperLogStr :: LogStr
            paperLogStr =
                let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime in
                toLogStr $ "[Paper]\t" ++ formattedDate ++ "\n" ++ show ie ++ "\n"


instance ErrorTProfile Prod GlobalErrorP where
    defaultError _ _= GlobalDefaultError
    defaultLogger _ _ context = (\_ _ logLevel logStr -> do
        let cfg = config context
        logDir :: String <- runGlobalMonadWithoutLog $ lookupRequiredGlobal @Prod cfg "log.global"
        let (fileNameList, header) =
                case logLevel of
                    LevelDebug -> (["debug.log", "all.log"], "[DEBUG]\n")
                    LevelInfo -> (["info.log", "info-error.log", "info-warn-error.log", "all.log"], "[INFO]\n")
                    LevelWarn -> (["warn.log", "info-warn-error.log", "all.log"], "[WARN]\n")
                    LevelError -> (["error.log", "info-error.log", "info-warn-error.log", "all.log"], "[ERROR]\n")
                    LevelOther _ -> (["other.log", "all.log"], "[OTHER]\n")
            filePathList = (\fileName -> logDir ++ "/global/" ++ fileName) <$> fileNameList
            content = Data.Text.Encoding.decodeUtf8 $ Data.ByteString.concat
                [Data.Text.Encoding.encodeUtf8 $ Data.Text.pack header, fromLogStr logStr]
        mapM_ (\filePath ->
            Data.Text.IO.appendFile filePath content) filePathList
        )
    defaultErrorLog _ _ ie currentTime =
        (defaultLoc, "GlobalErrorP", LevelError, globalLogStr)
        where
            globalLogStr :: LogStr
            globalLogStr =
                let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime in
                toLogStr $ "[Global]\t" ++ formattedDate ++ "\n" ++ show ie ++ "\n"