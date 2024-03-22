{-# LANGUAGE OverloadedStrings #-}

module DB(
    DB
  , PaperAuthDB(PaperAuthDB)
  , PaperAuthConn
  , PaperAuthPool
  , DBI(
        getPaperAuthPool'
      , runSqlPoolOneConnection
      , runSqlPoolOneConnectionGlobal
      , runSqlPoolOneConnectionNested
    )
) where

import Monad.ErrorT
import Monad.ProfileT
import Configurator
import Import
import GlobalMonad
import NestedMonad
import PaperMonad

import Database.Persist.Typed
import Database.Persist.MySQL
import Data.Configurator.Types

import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Text
import Data.Text.IO
import Data.Text.Encoding
import Data.ByteString
import Data.Time
import Data.Proxy
import System.Directory
import GHC.Stack

class (NestedMonadI p, ConfiguratorI p) => DBI p where
    paperAuthConnInfo' :: (HasCallStack, MonadUnliftIO m) => Config -> GlobalMonad p m ConnectInfo
    paperAuthConnInfo' = paperAuthConnInfo'Impl
    getPaperAuthPool' :: (HasCallStack, MonadUnliftIO m) => Config -> GlobalMonad p m PaperAuthPool
    getPaperAuthPool' = getPaperAuthPool'Impl
    runSqlPoolOneConnection :: (HasCallStack, DB db, MonadUnliftIO m) => (SqlFor db -> PaperMonad p m a) -> ConnectionPoolFor db -> PaperMonad p m a
    runSqlPoolOneConnection = runSqlPoolOneConnectionImpl
    runSqlPoolOneConnectionGlobal :: (HasCallStack, DB db, MonadUnliftIO m) => (SqlFor db -> GlobalMonad p m a) -> ConnectionPoolFor db -> GlobalMonad p m a
    runSqlPoolOneConnectionGlobal = runSqlPoolOneConnectionGlobalImpl
    runSqlPoolOneConnectionNested :: (HasCallStack, DB db, MonadUnliftIO m) => (SqlFor db -> NestedMonad p m a) -> ConnectionPoolFor db -> NestedMonad p m a
    runSqlPoolOneConnectionNested = runSqlPoolOneConnectionNestedImpl

paperAuthConnInfo'Impl :: (HasCallStack, DBI p, MonadUnliftIO m) => Config -> GlobalMonad p m ConnectInfo
paperAuthConnInfo'Impl cfg = do
    host <- lookupRequiredGlobal cfg "db.paper-auth.host"
    port <- lookupRequiredGlobal cfg "db.paper-auth.port"
    user <- lookupRequiredGlobal cfg "db.paper-auth.user"
    password <- lookupRequiredGlobal cfg "db.paper-auth.password"
    dbname <- lookupRequiredGlobal cfg "db.paper-auth.dbname"
    return defaultConnectInfo {
        connectHost = host
      , connectPort = port
      , connectUser = user
      , connectPassword = password
      , connectDatabase = dbname
    }

getPaperAuthPool'Impl :: forall p m. (HasCallStack, DBI p, MonadUnliftIO m) => Config -> GlobalMonad p m PaperAuthPool
getPaperAuthPool'Impl cfg = do
    paperAuthConnInfo <- paperAuthConnInfo' cfg
    globalLiftIOUnliftIO $ specializePool <$> (runLoggingT (createMySQLPool paperAuthConnInfo 16) logger)
    where
        logger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
        logger _ _ logLevel logStr = do
            currentTime <- getCurrentTime
            let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S\n" currentTime
            logDir :: String <- runGlobalMonad cfg $ lookupRequiredGlobal @p cfg "log"
            let (fileNameList, header) =
                    case logLevel of
                        LevelDebug -> (["debug.log", "all.log"], "[DEBUG]\t")
                        LevelInfo -> (["info.log", "info-error.log", "info-warn-error.log", "all.log"], "[INFO]\t")
                        LevelWarn -> (["warn.log", "info-warn-error.log", "all.log"], "[WARN]\t")
                        LevelError -> (["error.log", "info-error.log", "info-warn-error.log", "all.log"], "[ERROR]\t")
                        LevelOther _ -> (["other.log", "all.log"], "[OTHER]\t")
                dirName = logDir ++ "sql/"
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

runSqlPoolOneConnectionImpl :: forall db p m a. (HasCallStack, DBI p, DB db, MonadUnliftIO m) => (SqlFor db -> PaperMonad p m a) -> ConnectionPoolFor db -> PaperMonad p m a
runSqlPoolOneConnectionImpl inner pool = do
    profile <- ask
    safeErrorTToPaperMonad $ unsafeToSafeUnliftIO $ ErrorT $ LoggingT $ (\logger ->
        ExceptT $ runSqlPoolFor (ReaderT (\conn ->
            runExceptT $ catchE (inner' profile logger conn) (\e -> do
                runReaderT transactionUndo $ generalizeSqlBackend conn
                ExceptT $ return $ Left e)
            )) pool
            )
    where
        inner' :: HasCallStack => Proxy p -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> SqlFor db -> ExceptT PaperInnerError m a
        inner' profile logger conn = (runLoggingT $ unErrorT $ unSafeErrorT $ runReaderT (unProfileT $ unPaperMonad $ inner conn) profile) logger

runSqlPoolOneConnectionGlobalImpl :: forall db p m a. (HasCallStack, DBI p, DB db, MonadUnliftIO m) => (SqlFor db -> GlobalMonad p m a) -> ConnectionPoolFor db -> GlobalMonad p m a
runSqlPoolOneConnectionGlobalImpl inner pool = do
    profile <- ask
    safeErrorTToGlobalMonad $ unsafeToSafeUnliftIO $ ErrorT $ LoggingT $ (\logger ->
        ExceptT $ runSqlPoolFor (ReaderT (\conn ->
            runExceptT $ catchE (inner' profile logger conn) (\e -> do
                runReaderT transactionUndo $ generalizeSqlBackend conn
                ExceptT $ return $ Left e)
                )) pool
                )
    where
        inner' :: HasCallStack => Proxy p -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> SqlFor db -> ExceptT GlobalInnerError m a
        inner' profile logger conn = (runLoggingT $ unErrorT $ unSafeErrorT $ runReaderT (unProfileT $ unGlobalMonad $ inner conn) profile) logger

runSqlPoolOneConnectionNestedImpl :: forall db p m a. (HasCallStack, DBI p, DB db, MonadUnliftIO m) => (SqlFor db -> NestedMonad p m a) -> ConnectionPoolFor db -> NestedMonad p m a
runSqlPoolOneConnectionNestedImpl inner pool = do
    profile <- ask
    safeErrorTToNestedMonad $ unsafeToSafeUnliftIO $ ErrorT $ LoggingT $ (\logger ->
        ExceptT $ runSqlPoolFor (ReaderT (\conn ->
            runExceptT $ catchE (inner' profile logger conn) (\e -> do
                runReaderT transactionUndo $ generalizeSqlBackend conn
                ExceptT $ return $ Left e)
                )) pool
                )
    where
        inner' :: HasCallStack => Proxy p -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> SqlFor db -> ExceptT NestedInnerError m a
        inner' profile logger conn = (runLoggingT $ unErrorT $ unSafeErrorT $ runReaderT (unProfileT $ unNestedMonad $ inner conn) profile) logger