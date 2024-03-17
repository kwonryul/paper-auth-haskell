module OAuth2.Client.Model where

import Network.WebSockets.Connection

import Control.Concurrent

data OAuth2ClientSocketConnection = 
    OAuth2ClientWebSocketConnection {
        connection :: Connection 
      , sendLock :: MVar ()
      , closeShot :: MVar ()
      }
  | OAuth2ClientNativeSocketConnection {
        connection :: Connection
      , sendLock :: MVar ()
      , closeShot :: MVar ()
      }