module OAuth2.Client.Repository(
    OAuth2ClientRepositoryI(
        newConnection
      , saveState
      , closeConnection
      )
) where

import OAuth2.Client.Entity
import DB
import  Enum
import NestedMonad
import PaperMonad

import Database.Persist.Sql

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Time
import Data.Text
import GHC.Stack

class (NestedMonadI p, PaperMonadI p) => OAuth2ClientRepositoryI p where
    newConnection :: (HasCallStack, MonadUnliftIO m) => String -> Int -> UTCTime -> PaperAuthConn -> PaperMonad p m OAuth2ClientSocketId
    newConnection = newConnectionImpl
    saveState :: (HasCallStack, MonadUnliftIO m) => OAuth2ClientSocketId -> Text -> PaperAuthConn -> NestedMonad p m ()
    saveState = saveStateImpl
    closeConnection :: (HasCallStack, MonadUnliftIO m) => OAuth2ClientSocketId -> PaperAuthConn -> NestedMonad p m ()
    closeConnection = closeConnectionImpl

newConnectionImpl :: (HasCallStack, OAuth2ClientRepositoryI p, MonadUnliftIO m) => String -> Int -> UTCTime -> PaperAuthConn -> PaperMonad p m OAuth2ClientSocketId
newConnectionImpl host port currentTime conn =
    paperLiftUnliftIO $ runReaderT (insert $ OAuth2ClientSocket WebSocket host port Nothing currentTime) conn

saveStateImpl :: (HasCallStack, OAuth2ClientRepositoryI p, MonadUnliftIO m) => OAuth2ClientSocketId -> Text -> PaperAuthConn -> NestedMonad p m ()
saveStateImpl socketId state conn =
    nestedLiftUnliftIO $ runReaderT (update socketId [OAuth2ClientSocketState =. Just state]) conn

closeConnectionImpl :: (HasCallStack, OAuth2ClientRepositoryI p, MonadUnliftIO m) => OAuth2ClientSocketId -> PaperAuthConn -> NestedMonad p m ()
closeConnectionImpl socketId conn =
    nestedLiftUnliftIO $ runReaderT (delete socketId) conn