module OAuth2.Client.Model where

import Network.WebSockets.Connection

import Foreign.Ptr

import Control.Concurrent
import Data.Void

data OAuth2ClientSocketConnection = 
    OAuth2ClientWebSocketConnection {
        connection :: Connection 
      , sendLock :: MVar ()
      , closeShot :: MVar ()
      }
  | OAuth2ClientNativeSocketConnection {
        stream :: Ptr Void
      , sendLock :: MVar ()
      , oneShot :: Ptr Void
      }