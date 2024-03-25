module OAuth2.Client.Repository(
    OAuth2ClientRepositoryI(
        newConnection
      , newConnectionNested
      , getConnection
      , saveState
      , closeConnection
      , saveTokens
      )
) where

import OAuth2.Client.Entity
import DB
import Enum
import NestedMonad
import PaperMonad

import Database.Persist.Sql

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Time
import Data.Text
import Data.Proxy
import GHC.Stack

class (NestedMonadI p, PaperMonadI p) => OAuth2ClientRepositoryI p where
    newConnection :: (HasCallStack, MonadUnliftIO m) => SocketType -> String -> Int -> UTCTime -> PaperAuthConn -> PaperMonad p m OAuth2ClientSocketConnectionId
    newConnection = newConnectionImpl
    newConnectionNested :: (HasCallStack, MonadUnliftIO m) => SocketType -> String -> Int -> UTCTime -> PaperAuthConn -> NestedMonad p m OAuth2ClientSocketConnectionId
    newConnectionNested = newConnectionNestedImpl
    getConnection :: (HasCallStack, MonadUnliftIO m) => OAuth2ClientSocketConnectionId -> PaperAuthConn -> PaperMonad p m (Maybe OAuth2ClientSocketConnection)
    getConnection = getConnectionImpl
    saveState :: HasCallStack => Proxy p -> OAuth2ClientSocketConnectionId -> Text -> PaperAuthConn -> IO ()
    saveState = saveStateImpl
    closeConnection :: (HasCallStack, MonadUnliftIO m) => OAuth2ClientSocketConnectionId -> PaperAuthConn -> NestedMonad p m ()
    closeConnection = closeConnectionImpl
    saveTokens :: (HasCallStack, MonadUnliftIO m) => OAuth2ClientSocketConnectionId -> Text -> Text -> UTCTime -> PaperAuthConn -> PaperMonad p m ()
    saveTokens = saveTokensImpl

newConnectionImpl :: (HasCallStack, OAuth2ClientRepositoryI p, MonadUnliftIO m) => SocketType -> String -> Int -> UTCTime -> PaperAuthConn -> PaperMonad p m OAuth2ClientSocketConnectionId
newConnectionImpl socketType host port currentTime conn =
    paperLiftUnliftIO $ runReaderT (insert $ OAuth2ClientSocketConnection socketType host port Nothing Nothing Nothing currentTime) conn

newConnectionNestedImpl :: (HasCallStack, OAuth2ClientRepositoryI p, MonadUnliftIO m) => SocketType -> String -> Int -> UTCTime -> PaperAuthConn -> NestedMonad p m OAuth2ClientSocketConnectionId
newConnectionNestedImpl socketType host port currentTime conn =
    nestedLiftUnliftIO $ runReaderT (insert $ OAuth2ClientSocketConnection socketType host port Nothing Nothing Nothing currentTime) conn

getConnectionImpl :: (HasCallStack, OAuth2ClientRepositoryI p, MonadUnliftIO m) => OAuth2ClientSocketConnectionId -> PaperAuthConn -> PaperMonad p m (Maybe OAuth2ClientSocketConnection)
getConnectionImpl socketId conn =
    paperLiftUnliftIO $ runReaderT (get socketId) conn

saveStateImpl :: (HasCallStack, OAuth2ClientRepositoryI p) => Proxy p -> OAuth2ClientSocketConnectionId -> Text -> PaperAuthConn -> IO ()
saveStateImpl _ socketId state conn  =
    runReaderT (update socketId [OAuth2ClientSocketConnectionState =. Just state]) conn

closeConnectionImpl :: (HasCallStack, OAuth2ClientRepositoryI p, MonadUnliftIO m) => OAuth2ClientSocketConnectionId -> PaperAuthConn -> NestedMonad p m ()
closeConnectionImpl socketId conn =
    nestedLiftUnliftIO $ runReaderT (delete socketId) conn

saveTokensImpl :: (HasCallStack, OAuth2ClientRepositoryI p, MonadUnliftIO m) => OAuth2ClientSocketConnectionId -> Text -> Text -> UTCTime -> PaperAuthConn -> PaperMonad p m ()
saveTokensImpl socketId accessToken refreshToken currentUTC conn =
    paperLiftUnliftIO $ runReaderT (update socketId [
        OAuth2ClientSocketConnectionAccessToken =. Just accessToken
      , OAuth2ClientSocketConnectionRefreshToken =. Just refreshToken
      , OAuth2ClientSocketConnectionIat =. currentUTC
      ]) conn