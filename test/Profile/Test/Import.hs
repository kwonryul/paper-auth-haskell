{-# LANGUAGE OverloadedStrings #-}

module Profile.Test.Import(
    Test
  ) where

import Monad.ProfileT

import Monad.ErrorT
import CallStack
import Configurator
import Context
import GlobalMonad
import PaperMonad

import Control.Monad.Logger
import Data.Time
import Data.Text
import Data.Text.IO
import Data.Text.Encoding
import Data.ByteString
import System.Directory

data Test

instance Profile Test

instance ErrorTI Test
instance CallStackI Test
instance ConfiguratorI Test
instance GlobalMonadI Test
instance PaperMonadI Test

instance ErrorTProfile Test PaperErrorP where
    defaultError _ _ = PaperDefaultError
    defaultLogger _ _ context = (\_ _ logLevel logStr -> do
        currentTime <- getCurrentTime
        let cfg = config context
            formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S\n" currentTime
        logDir :: String <- runGlobalMonadWithoutLog $ lookupRequiredGlobal @Test cfg "log.paper"
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

instance ErrorTProfile Test GlobalErrorP where
    defaultError _ _= GlobalDefaultError
    defaultLogger _ _ context = (\_ _ logLevel logStr -> do
        currentTime <- getCurrentTime
        let cfg = config context
            formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S\n" currentTime
        logDir :: String <- runGlobalMonadWithoutLog $ lookupRequiredGlobal @Test cfg "log.global"
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