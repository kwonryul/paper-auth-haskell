{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module OAuth2.Client.Service(
    OAuth2ClientServiceI(
        webSocket
      , issueJWT
      , finalize
      )
) where

import qualified JWT.ExService
import JWT.ExService(JWTExServiceI)
import qualified OAuth2.Client.GRpc.ExService
import OAuth2.Client.GRpc.ExService(OAuth2ClientGRpcExServiceI)
import qualified OAuth2.Client.ExService
import OAuth2.Client.ExService(OAuth2ClientExServiceI)
import qualified OAuth2.Client.Repository
import OAuth2.Client.Repository(OAuth2ClientRepositoryI)
import qualified User.Repository
import User.Repository(UserRepositoryI)

import JWT.Model
import JWT.Util
import OAuth2.Client.Entity
import OAuth2.Client.HTML
import OAuth2.Client.Model
import OAuth2.Client.Util
import CallStack
import Configurator
import Context
import DB
import Enum
import Import
import NestedMonad
import PaperMonad

import Servant
import Text.Blaze.Html
import Database.Persist.Sql
import Database.Persist.Typed
import Network.WebSockets
import Web.Cookie
import Web.JWT
import Data.Configurator.Types

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Concurrent
import Control.Exception
import Data.Map
import Data.Text
import Data.Time
import GHC.Stack

class (
    DBI p
  , OAuth2ClientGRpcExServiceI p
  , JWTExServiceI p
  , JWTUtilI p
  , OAuth2ClientExServiceI p
  , OAuth2ClientUtilI p
  , OAuth2ClientRepositoryI p
  , UserRepositoryI p
  , NestedMonadI p) => OAuth2ClientServiceI p where
    webSocket :: (HasCallStack, MonadUnliftIO m) => Context.Context -> OAuth2ClientSocketConnections -> PendingConnection -> PaperAuthPool -> PaperMonad p m ()
    webSocket = webSocketImpl
    issueJWT :: (HasCallStack, MonadUnliftIO m) => Config -> EncodeSigner -> AuthenticationType -> String -> String -> PaperAuthPool -> PaperMonad p m (Servant.Headers '[Header "Set-Cookie" SetCookie] Html)
    issueJWT = issueJWTImpl
    issueJWT' :: (HasCallStack, MonadUnliftIO m) => Config -> EncodeSigner -> AuthenticationType -> String -> String -> PaperAuthConn -> PaperMonad p m (Servant.Headers '[Header "Set-Cookie" SetCookie] Html)
    issueJWT' = issueJWT'Impl
    finalize :: (HasCallStack, MonadUnliftIO m) => String -> PaperAuthPool -> PaperMonad p m NoContent
    finalize = finalizeImpl
    finalize' :: (HasCallStack, MonadUnliftIO m) => String -> PaperAuthConn -> PaperMonad p m NoContent
    finalize' = finalize'Impl

webSocketImpl :: forall p m. (HasCallStack, OAuth2ClientServiceI p, MonadUnliftIO m) => Context.Context -> OAuth2ClientSocketConnections -> PendingConnection -> PaperAuthPool -> PaperMonad p m ()
webSocketImpl ctx socketConnections'' socketConn' pool = do
    profile <- ask
    currentTime <- paperLiftIOUnliftIO getCurrentTime
    socketConn <- paperLiftIOUnliftIO $ acceptRequest socketConn'
    host <- lookupRequired (config ctx) "host"
    port <- lookupRequired (config ctx) "port.oauth2-client-socket"
    socketId <- runSqlPoolOneConnection (OAuth2.Client.Repository.newConnection WebSocket host port currentTime) pool
    sendLock <- paperLiftIOUnliftIO newEmptyMVar
    closeShot <- paperLiftIOUnliftIO newEmptyMVar
    socketConnections <- paperLiftIOUnliftIO $ takeMVar socketConnections''
    paperLiftIOUnliftIO $ catch (nestedLog profile ctx $ putMVar socketConnections'' $ Data.Map.insert
        (fromIntegral $ fromSqlKeyFor socketId)
        (OAuth2ClientWebSocketConnection socketConn sendLock closeShot)
        socketConnections
        ) $ \(e :: SomeException) -> do
            nestedLog profile ctx $ putMVar socketConnections'' socketConnections
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
            socketConnections' <- nestedLog profile ctx $ takeMVar socketConnections''
            catch (nestedLog profile ctx $ putMVar socketConnections'' $ Data.Map.delete
                (fromIntegral $ fromSqlKeyFor socketId)
                socketConnections'
                ) $ \(e :: SomeException) -> do
                    nestedLog profile ctx $ putMVar socketConnections'' socketConnections'
                    throw e
            )
            (\_ -> nestedLog profile ctx $ takeMVar closeShot)

    where
        receiveLoop :: Connection -> IO ()
        receiveLoop socketConn = do
            msg <- receive socketConn
            case msg of
                ControlMessage (Close _ _) -> return ()
                _ -> receiveLoop socketConn

issueJWTImpl :: (HasCallStack, OAuth2ClientServiceI p, MonadUnliftIO m) => Config -> EncodeSigner -> AuthenticationType -> String -> String -> PaperAuthPool -> PaperMonad p m (Servant.Headers '[Header "Set-Cookie" SetCookie] Html)
issueJWTImpl config encodeSigner authenticationType code state pool =
    runSqlPoolOneConnection (issueJWT' config encodeSigner authenticationType code state) pool

issueJWT'Impl :: (HasCallStack, OAuth2ClientServiceI p, MonadUnliftIO m) => Config -> EncodeSigner -> AuthenticationType -> String -> String -> PaperAuthConn -> PaperMonad p m (Servant.Headers '[Header "Set-Cookie" SetCookie] Html)
issueJWT'Impl config encodeSigner authenticationType code state conn = do
    profile <- ask
    currentUTC <- paperLiftIOUnliftIO getCurrentTime
    socketId <- maybeToPaperMonad (getSocketIdFromState profile state) $ PaperError "state invalid" (err400 { errBody = "state invalid" }) $ callStack' profile
    socketConnection' <- OAuth2.Client.Repository.getConnection (toSqlKeyFor $ fromIntegral socketId) conn
    OAuth2ClientSocketConnection {
        oAuth2ClientSocketConnectionType
      , oAuth2ClientSocketConnectionState
    } <- maybeToPaperMonad socketConnection' $ PaperError "socket connection not found" (err500 { errBody = "state invalid or ws connection closed" }) $ callStack' profile
    paperAssert (
        case oAuth2ClientSocketConnectionState of
            Just state' -> state' == Data.Text.pack state
            Nothing -> False
        ) $ PaperError "state invalid" (err400 { errBody = "state invalid" }) $ callStack' profile
    identifier <- OAuth2.Client.ExService.getIdentifier config authenticationType code state
    userEntityList <- User.Repository.findByAuthTypeAndIdentifier authenticationType identifier conn
    userId <- case userEntityList of
        [Entity userId _] -> return userId
        [] -> User.Repository.newUser authenticationType Nothing Nothing Nothing (Just identifier) currentUTC conn
        _ -> toPaperMonad $ PaperError "authenticationType * identifier duplicate" (err500 { errBody = "database error" }) $ callStack' profile
    preAuthenticatedUser <- User.Repository.getPreAuthenticatedUser userId conn
    JWTDTO { accessToken, refreshToken } <- JWT.ExService.issueJWT config encodeSigner preAuthenticatedUser currentUTC conn
    OAuth2.Client.Repository.saveTokens (toSqlKeyFor $ fromIntegral socketId) accessToken refreshToken currentUTC conn
    let cookie = generateRefreshTokenCookie profile refreshToken
    case oAuth2ClientSocketConnectionType of
        WebSocket -> do
            return $ addHeader cookie $ issueJWTHtml state
        NativeSocket -> do
            return $ addHeader cookie $ issueJWTHtml state

finalizeImpl :: (HasCallStack, OAuth2ClientServiceI p, MonadUnliftIO m) => String -> PaperAuthPool -> PaperMonad p m NoContent
finalizeImpl state pool =
    runSqlPoolOneConnection (finalize' state) pool

finalize'Impl :: (HasCallStack, OAuth2ClientServiceI p, MonadUnliftIO m) => String -> PaperAuthConn -> PaperMonad p m NoContent
finalize'Impl state conn = do
    profile <- ask
    socketId <- maybeToPaperMonad (getSocketIdFromState profile state) $ PaperError "state invalid" (err400 { errBody = "state invalid" }) $ callStack' profile
    socketConnection' <- OAuth2.Client.Repository.getConnection (toSqlKeyFor $ fromIntegral socketId) conn
    OAuth2ClientSocketConnection {
        oAuth2ClientSocketConnectionType
      , oAuth2ClientSocketConnectionHost
      , oAuth2ClientSocketConnectionPort
      , oAuth2ClientSocketConnectionState
      , oAuth2ClientSocketConnectionAccessToken
      , oAuth2ClientSocketConnectionRefreshToken
    } <- maybeToPaperMonad socketConnection' $ PaperError "socket connection not found" (err500 { errBody = "state invalid or ws connection closed" }) $ callStack' profile
    paperAssert (
        case oAuth2ClientSocketConnectionState of
            Just state' -> state' == Data.Text.pack state
            Nothing -> False
        ) $ PaperError "state invalid" (err400 { errBody = "state invalid" }) $ callStack' profile
    accessToken <- maybeToPaperMonad oAuth2ClientSocketConnectionAccessToken $ PaperError "accessToken missing" (err500 { errBody = "database error" }) $ callStack' profile
    refreshToken <- maybeToPaperMonad oAuth2ClientSocketConnectionRefreshToken $ PaperError "refreshToken missing" (err500 { errBody = "database error" }) $ callStack' profile
    case oAuth2ClientSocketConnectionType of
        WebSocket ->
            OAuth2.Client.GRpc.ExService.sendToken oAuth2ClientSocketConnectionHost oAuth2ClientSocketConnectionPort socketId accessToken Nothing
        NativeSocket ->
            OAuth2.Client.GRpc.ExService.sendToken oAuth2ClientSocketConnectionHost oAuth2ClientSocketConnectionPort socketId accessToken $ Just refreshToken
    return NoContent