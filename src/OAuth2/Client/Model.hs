module OAuth2.Client.Model(
    OAuth2ClientSocketConnection(
        OAuth2ClientWebSocketConnection
      , connection
      , sendLock
      , closeShot
      , OAuth2ClientNativeSocketConnection
      )
) where

import Network.WebSockets.Connection

import Control.Concurrent

data OAuth2ClientSocketConnection =
    OAuth2ClientWebSocketConnection {
        connection :: Connection 
      , sendLock :: MVar ()
      , closeShot :: MVar ()
      }
  | OAuth2ClientNativeSocketConnection