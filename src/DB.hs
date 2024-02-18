{-# LANGUAGE OverloadedStrings #-}

module DB(
    DB
  , PaperAuthDB(PaperAuthDB)
  , PaperAuthConn
  , PaperAuthPool
  , getPaperAuthPool'
) where

import Import
import Configurator
import Exception

import Database.Persist.Typed
import Database.Persist.MySQL
import Data.Configurator.Types

import Control.Monad.Trans.Either
import Control.Monad.Trans.Resource
import Control.Monad.Logger

data PaperAuthDB = PaperAuthDB deriving Show
instance DB PaperAuthDB
type PaperAuthConn = SqlFor PaperAuthDB
type PaperAuthPool = ConnectionPoolFor PaperAuthDB

paperAuthConnInfo' :: Config -> PaperEitherT IO ConnectInfo
paperAuthConnInfo' config = do
    host <- lookupConfig config "db.paper-auth.host"
    port <- lookupConfig config "db.paper-auth.port"
    user <- lookupConfig config "db.paper-auth.user"
    password <- lookupConfig config "db.paper-auth.password"
    dbname <- lookupConfig config "db.paper-auth.dbname"
    return defaultConnectInfo {
        connectHost = host
      , connectPort = port
      , connectUser = user
      , connectPassword = password
      , connectDatabase = dbname
    }

getPaperAuthPool' :: (MonadUnliftIO m, MonadLoggerIO m) => Config -> PaperEitherT m PaperAuthPool
getPaperAuthPool' config = do
    paperAuthConnInfo <- liftIOEitherT $ paperAuthConnInfo' config
    EitherT $ Right . specializePool <$> createMySQLPool paperAuthConnInfo 8