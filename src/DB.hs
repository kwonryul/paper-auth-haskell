{-# LANGUAGE OverloadedStrings #-}

module DB(
    DB
  , PaperAuthDB(PaperAuthDB)
  , PaperAuthConn
  , PaperAuthPool
  , getPaperAuthPool'
) where

import Configurator
import GlobalError
import Import

import Database.Persist.Typed
import Database.Persist.MySQL
import Data.Configurator.Types

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import GHC.Stack

data PaperAuthDB = PaperAuthDB deriving Show
instance DB PaperAuthDB
type PaperAuthConn = SqlFor PaperAuthDB
type PaperAuthPool = ConnectionPoolFor PaperAuthDB

paperAuthConnInfo' :: (HasCallStack, MonadUnliftIO m) => Config -> GlobalExceptT m ConnectInfo
paperAuthConnInfo' config = do
    host <- lookupRequiredGlobal config "db.paper-auth.host"
    port <- lookupRequiredGlobal config "db.paper-auth.port"
    user <- lookupRequiredGlobal config "db.paper-auth.user"
    password <- lookupRequiredGlobal config "db.paper-auth.password"
    dbname <- lookupRequiredGlobal config "db.paper-auth.dbname"
    return defaultConnectInfo {
        connectHost = host
      , connectPort = port
      , connectUser = user
      , connectPassword = password
      , connectDatabase = dbname
    }

getPaperAuthPool' :: (HasCallStack, MonadUnliftIO m) => Config -> GlobalExceptT m PaperAuthPool
getPaperAuthPool' config = do
    paperAuthConnInfo <- paperAuthConnInfo' config
    globalLiftIO $ specializePool <$> (runStderrLoggingT $ createMySQLPool paperAuthConnInfo 8)