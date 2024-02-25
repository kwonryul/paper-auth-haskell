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
import GlobalError

import Database.Persist.Typed
import Database.Persist.MySQL
import Data.Configurator.Types

import Control.Monad.Logger
import GHC.Stack

data PaperAuthDB = PaperAuthDB deriving Show
instance DB PaperAuthDB
type PaperAuthConn = SqlFor PaperAuthDB
type PaperAuthPool = ConnectionPoolFor PaperAuthDB

paperAuthConnInfo' :: HasCallStack => Config -> GlobalExceptT IO ConnectInfo
paperAuthConnInfo' config = do
    host <- lookupConfigGlobal config "db.paper-auth.host"
    port <- lookupConfigGlobal config "db.paper-auth.port"
    user <- lookupConfigGlobal config "db.paper-auth.user"
    password <- lookupConfigGlobal config "db.paper-auth.password"
    dbname <- lookupConfigGlobal config "db.paper-auth.dbname"
    return defaultConnectInfo {
        connectHost = host
      , connectPort = port
      , connectUser = user
      , connectPassword = password
      , connectDatabase = dbname
    }

getPaperAuthPool' :: HasCallStack => Config -> GlobalExceptT IO PaperAuthPool
getPaperAuthPool' config = do
    paperAuthConnInfo <- paperAuthConnInfo' config
    globalLiftIO $ specializePool <$> (runStderrLoggingT $ createMySQLPool paperAuthConnInfo 8)