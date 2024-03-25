{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module OAuth2.Client.GRpc.Controller(
    OAuth2ClientGRpcControllerI(
        saveConnectionHs
      , cleanConnectionHs
      , sendTokenAndCloseHs
      )
) where

import qualified OAuth2.Client.Repository
import OAuth2.Client.Repository(OAuth2ClientRepositoryI)

import OAuth2.Client.GRpc.DTO
import OAuth2.Client.Entity
import OAuth2.Client.Model
import OAuth2.Client.Util
import CallStack
import Configurator
import DB
import Enum
import Import
import NestedMonad

import Database.Persist.Typed
import Database.Persist.MySQL
import Network.WebSockets
import Data.Aeson
import Data.Configurator.Types

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc

import Control.Concurrent.MVar
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Exception
import Data.Map
import Data.Text
import Data.Time
import Data.Void
import Data.Proxy
import GHC.Stack

foreign import ccall "send_state_native_c" sendStateNativeC :: Ptr Void -> CString -> IO CString
foreign import ccall "send_token_and_close_native_c" sendTokenAndCloseNativeC :: Ptr Void -> Ptr Void -> CString -> CString -> Int -> IO CString

class (OAuth2ClientRepositoryI p, OAuth2ClientUtilI p, ConfiguratorI p, DBI p, NestedMonadI p) => OAuth2ClientGRpcControllerI p where
    saveConnectionHs :: HasCallStack => Proxy p -> Config -> OAuth2ClientSocketConnections -> PaperAuthPool -> Ptr Void -> Ptr Void -> IO Int
    saveConnectionHs = saveConnectionHsImpl
    saveConnectionHs' :: (HasCallStack, MonadUnliftIO m) => Config -> OAuth2ClientSocketConnections -> Ptr Void -> Ptr Void -> PaperAuthConn -> NestedMonad p m Int
    saveConnectionHs' = saveConnectionHs'Impl
    cleanConnectionHs :: HasCallStack => Proxy p -> Config -> OAuth2ClientSocketConnections -> PaperAuthPool -> Int -> IO CString
    cleanConnectionHs = cleanConnectionHsImpl
    cleanConnectionHs' :: (HasCallStack, MonadUnliftIO m) => Config -> OAuth2ClientSocketConnections -> Int -> PaperAuthConn -> NestedMonad p m CString
    cleanConnectionHs' = cleanConnectionHs'Impl
    sendTokenAndCloseHs :: HasCallStack => Proxy p -> Config -> OAuth2ClientSocketConnections -> Int -> CString -> CString -> IO CString
    sendTokenAndCloseHs = sendTokenAndCloseHsImpl

saveConnectionHsImpl :: forall p. (HasCallStack, OAuth2ClientGRpcControllerI p) => Proxy p -> Config -> OAuth2ClientSocketConnections -> PaperAuthPool -> Ptr Void -> Ptr Void -> IO Int
saveConnectionHsImpl _ cfg socketConnections pool stream oneShot = do
    runNestedMonad cfg $ runSqlPoolOneConnectionNested (saveConnectionHs' @p cfg socketConnections stream oneShot) pool

saveConnectionHs'Impl :: forall p m. (HasCallStack, OAuth2ClientGRpcControllerI p, MonadUnliftIO m) => Config -> OAuth2ClientSocketConnections -> Ptr Void -> Ptr Void -> PaperAuthConn -> NestedMonad p m Int
saveConnectionHs'Impl cfg socketConnections'' stream oneShot conn = do
    profile <- ask
    currentTime <- nestedLiftIOUnliftIO getCurrentTime
    host <- lookupRequiredNested cfg "host"
    port <- lookupRequiredNested cfg "port.oauth2-client-socket"
    socketId <- OAuth2.Client.Repository.newConnectionNested NativeSocket host port currentTime conn
    sendLock <- nestedLiftIOUnliftIO newEmptyMVar
    socketConnections <- nestedLiftIOUnliftIO $ takeMVar socketConnections''
    nestedLiftIOUnliftIO $ catch (nestedLog profile cfg $ putMVar socketConnections'' $ Data.Map.insert
        (fromIntegral $ fromSqlKeyFor socketId)
        (OAuth2ClientNativeSocketConnection stream sendLock oneShot)
        socketConnections
        ) $ \(e :: SomeException) -> do
            nestedLog profile cfg $ putMVar socketConnections'' socketConnections
            throw e
    nestedLiftIOUnliftIO $ catch (do
        state <- nestedLog profile cfg $ generateState profile (fromIntegral $ fromSqlKeyFor socketId)
        res <- nestedLog profile cfg $ bracket (
            bracket
                (nestedLog profile cfg $ newCString $ Data.Text.unpack state)
                (nestedLog profile cfg . free)
                (\stateCString -> sendStateNativeC stream stateCString)
            ) (nestedLog profile cfg . free) (nestedLog profile cfg . peekCString)
        case res of
            "OK" -> do
                nestedLog profile cfg $ putMVar sendLock()
                nestedLog profile cfg $ OAuth2.Client.Repository.saveState profile socketId state conn
                return $ fromIntegral $ fromSqlKeyFor socketId
            err -> runNestedMonad @p cfg $ toNestedMonad $ NestedError err $ callStack' profile
        ) (\(e :: SomeException) -> do
            cleanUp profile socketId
            throw e)
    where
        cleanUp :: HasCallStack => Proxy p -> OAuth2ClientSocketConnectionId -> IO ()
        cleanUp profile socketId = do
            runReaderT transactionUndo $ generalizeSqlBackend conn
            socketConnections' <- nestedLog profile cfg $ takeMVar socketConnections''
            catch (nestedLog profile cfg $ putMVar socketConnections'' $ Data.Map.delete
                (fromIntegral $ fromSqlKeyFor socketId)
                socketConnections'
                ) $ \(e :: SomeException) -> do
                    nestedLog profile cfg $ putMVar socketConnections'' socketConnections'
                    throw e

cleanConnectionHsImpl :: forall p. (HasCallStack, OAuth2ClientGRpcControllerI p) => Proxy p -> Config -> OAuth2ClientSocketConnections -> PaperAuthPool -> Int -> IO CString
cleanConnectionHsImpl _ cfg socketConnections pool socketId = do
    runNestedMonad cfg $ runSqlPoolOneConnectionNested (cleanConnectionHs' @p cfg socketConnections socketId) pool

cleanConnectionHs'Impl :: (HasCallStack, OAuth2ClientGRpcControllerI p, MonadUnliftIO m) => Config -> OAuth2ClientSocketConnections -> Int -> PaperAuthConn -> NestedMonad p m CString
cleanConnectionHs'Impl cfg socketConnections'' socketId' conn = do
    profile <- ask
    let socketId = toSqlKeyFor $ fromIntegral socketId'
    OAuth2.Client.Repository.closeConnection socketId conn
    socketConnections' <- nestedLiftIOUnliftIO $ takeMVar socketConnections''
    nestedLiftIOUnliftIO $ catch (nestedLog profile cfg $ putMVar socketConnections'' $ Data.Map.delete
        socketId'
        socketConnections'
        ) $ \(e :: SomeException) -> do
            nestedLog profile cfg $ putMVar socketConnections'' socketConnections'
            throw e
    ok <- nestedLiftIOUnliftIO $ newCString "OK"
    return ok


sendTokenAndCloseHsImpl :: (HasCallStack, OAuth2ClientGRpcControllerI p) => Proxy p -> Config -> OAuth2ClientSocketConnections -> Int -> CString -> CString -> IO CString
sendTokenAndCloseHsImpl profile cfg socketConnections' socketId accessToken' refreshToken'' = do
    accessToken <- nestedLog profile cfg $ Data.Text.pack <$> peekCString accessToken'
    refreshToken' <- nestedLog profile cfg $ peekCString refreshToken''
    let refreshToken = if refreshToken' == "" then Nothing else Just $ Data.Text.pack refreshToken'
    socketConnections <- nestedLog profile cfg $ readMVar socketConnections'
    case Data.Map.lookup socketId socketConnections of
        Just (OAuth2ClientWebSocketConnection {
            connection
          , sendLock
          , closeShot
        }) -> do
            bracket (nestedLog profile cfg $ takeMVar sendLock) (\_ -> nestedLog profile cfg $ putMVar sendLock ()) (\_ ->
                nestedLog profile cfg $ sendTextData connection $ encode $ IssueJWTResDTO accessToken refreshToken
                )
            nestedLog profile cfg $ putMVar closeShot ()
            ok <- nestedLog profile cfg $ newCString "OK"
            return ok
        Just (OAuth2ClientNativeSocketConnection {
            stream
          , sendLock
          , oneShot
        }) ->
            bracket
                (nestedLog profile cfg $ takeMVar sendLock)
                (\_ -> nestedLog profile cfg $ putMVar sendLock ())
                (\_ ->
                    nestedLog profile cfg $ sendTokenAndCloseNativeC stream oneShot accessToken' refreshToken'' socketId)
        Nothing -> do
            err <- nestedLog profile cfg $ newCString "socket closed or invalid"
            return err