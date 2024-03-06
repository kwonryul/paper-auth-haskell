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
    )
) where

import qualified Configurator
import Configurator(ConfiguratorI)
import GlobalMonad
import PaperMonad
import Monad.ProfileT
import Monad.ErrorT
import Import

import Database.Persist.Typed
import Database.Persist.MySQL
import Data.Configurator.Types

import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Proxy
import GHC.Stack

class ConfiguratorI p => DBI p where
    paperAuthConnInfo' :: (HasCallStack, MonadUnliftIO m) => Config -> GlobalMonad p m ConnectInfo
    paperAuthConnInfo' = paperAuthConnInfo'Impl
    getPaperAuthPool' :: (HasCallStack, MonadUnliftIO m) => Config -> GlobalMonad p m PaperAuthPool
    getPaperAuthPool' = getPaperAuthPool'Impl
    runSqlPoolOneConnection :: forall db m a. (HasCallStack, DB db, MonadUnliftIO m) => (SqlFor db -> PaperMonad p m a) -> ConnectionPoolFor db -> PaperMonad p m a
    runSqlPoolOneConnection = runSqlPoolOneConnectionImpl
    runSqlPoolOneConnectionGlobal :: forall db m a. (HasCallStack, DB db, MonadUnliftIO m) => (SqlFor db -> GlobalMonad p m a) -> ConnectionPoolFor db -> GlobalMonad p m a
    runSqlPoolOneConnectionGlobal = runSqlPoolOneConnectionGlobalImpl

paperAuthConnInfo'Impl :: (HasCallStack, DBI p, MonadUnliftIO m) => Config -> GlobalMonad p m ConnectInfo
paperAuthConnInfo'Impl config = do
    host <- Configurator.lookupRequiredGlobal config "db.paper-auth.host"
    port <- Configurator.lookupRequiredGlobal config "db.paper-auth.port"
    user <- Configurator.lookupRequiredGlobal config "db.paper-auth.user"
    password <- Configurator.lookupRequiredGlobal config "db.paper-auth.password"
    dbname <- Configurator.lookupRequiredGlobal config "db.paper-auth.dbname"
    return defaultConnectInfo {
        connectHost = host
      , connectPort = port
      , connectUser = user
      , connectPassword = password
      , connectDatabase = dbname
    }

getPaperAuthPool'Impl :: (HasCallStack, DBI p, MonadUnliftIO m) => Config -> GlobalMonad p m PaperAuthPool
getPaperAuthPool'Impl config = do
    paperAuthConnInfo <- paperAuthConnInfo' config
    globalLiftIOUnliftIO $ specializePool <$> (runStderrLoggingT $ createMySQLPool paperAuthConnInfo 8)

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
        inner' :: (HasCallStack, DB db, MonadUnliftIO m) => Proxy p -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> SqlFor db -> ExceptT PaperInnerError m a
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
        inner' :: (HasCallStack, DB db, MonadUnliftIO m) => Proxy p -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> SqlFor db -> ExceptT GlobalInnerError m a
        inner' profile logger conn = (runLoggingT $ unErrorT $ unSafeErrorT $ runReaderT (unProfileT $ unGlobalMonad $ inner conn) profile) logger