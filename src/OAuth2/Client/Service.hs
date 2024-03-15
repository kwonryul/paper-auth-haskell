{-# LANGUAGE OverloadedStrings #-}

module OAuth2.Client.Service(
    OAuth2ClientServiceI(
        socket
      )
) where

import qualified OAuth2.Client.Repository
import OAuth2.Client.Repository(OAuth2ClientRepositoryI)

import OAuth2.Client.Model
import OAuth2.Client.Util
import Configurator
import Context
import DB
import Import
import NestedMonad
import PaperMonad

import Database.Persist.Typed
import Network.WebSockets

import Control.Monad.IO.Unlift
import Control.Concurrent
import Control.Exception
import Data.Map
import Data.Proxy
import Data.Time
import GHC.Stack

class (DBI p, NestedMonadI p, OAuth2ClientUtilI p, OAuth2ClientRepositoryI p) => OAuth2ClientServiceI p where
    socket :: (HasCallStack, MonadUnliftIO m) => Context.Context -> OAuth2ClientSocketConnections -> PendingConnection -> PaperAuthPool -> PaperMonad p m ()
    socket = socketImpl

socketImpl :: forall p m. (HasCallStack, OAuth2ClientServiceI p, MonadUnliftIO m) => Context.Context -> OAuth2ClientSocketConnections -> PendingConnection -> PaperAuthPool -> PaperMonad p m ()
socketImpl ctx socketConnections' socketConn' pool = do
    currentTime <- paperLiftIOUnliftIO getCurrentTime
    socketConn <- paperLiftIOUnliftIO $ acceptRequest socketConn'
    host <- lookupRequired (config ctx) "host"
    port <- lookupRequired (config ctx) "port.thrift"
    socketId <- runSqlPoolOneConnection (OAuth2.Client.Repository.newConnection host port currentTime) pool
    sendLock <- paperLiftIOUnliftIO newEmptyMVar
    closeShot <- paperLiftIOUnliftIO newEmptyMVar
    socketConnections <- paperLiftIOUnliftIO $ takeMVar socketConnections'
    paperLiftIOUnliftIO $ catch (nestedLog profile ctx $ putMVar socketConnections' $ insert
        (fromIntegral $ fromSqlKeyFor socketId)
        (OAuth2ClientWebSocketConnection socketConn sendLock closeShot)
        socketConnections
        ) $ \(e :: SomeException) -> do
            nestedLog profile ctx $ putMVar socketConnections' socketConnections
            throw e
    _ <- paperLiftIOUnliftIO $ forkIO $ do
        bracket (return ()) (\_ -> do
            _ <- nestedLog profile ctx $ tryPutMVar closeShot ()
            return ()
            )
            (const $ receiveLoop socketConn)
    paperLiftIOUnliftIO $ catch (do
        state <- nestedLog profile ctx $ generateState profile (fromIntegral $ fromSqlKeyFor socketId)
        nestedLog profile ctx $ sendTextData socketConn state
        runNestedMonad @p ctx $ runSqlPoolOneConnectionNested (OAuth2.Client.Repository.saveState socketId state) pool) (\(_ :: SomeException) -> return ())
    paperLiftIOUnliftIO $ putMVar sendLock ()
    paperLiftIOUnliftIO $ withPingThread socketConn 30 (return ()) $ do
        bracket (return ()) (\_ -> do
            runNestedMonad @p ctx $ runSqlPoolOneConnectionNested (OAuth2.Client.Repository.closeConnection socketId) pool
            socketConnections'' <- nestedLog profile ctx $ takeMVar socketConnections'
            catch (nestedLog profile ctx $ putMVar socketConnections' $ delete
                (fromIntegral $ fromSqlKeyFor socketId)
                socketConnections''
                ) $ \(e :: SomeException) -> do
                    nestedLog profile ctx $ putMVar socketConnections' socketConnections''
                    throw e
            )
            (\_ -> nestedLog profile ctx $ takeMVar closeShot)

    where
        profile :: Proxy p
        profile = Proxy
        receiveLoop :: Connection -> IO ()
        receiveLoop socketConn = do
            msg <- nestedLog profile ctx $ receive socketConn
            case msg of
                ControlMessage (Close _ _) -> return ()
                _ -> receiveLoop socketConn