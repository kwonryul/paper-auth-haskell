{-# LANGUAGE DataKinds #-}

module OAuth2.Client.Controller(
    API
  , OAuth2ClientControllerI(
        server
      )
) where

import qualified OAuth2.Client.Service
import OAuth2.Client.Service(OAuth2ClientServiceI)

import Context
import PaperMonad

import Servant
import Servant.API.WebSocket
import Network.WebSockets.Connection

import GHC.Stack

type API =
        "socket" :> ConnectSocket

type ConnectSocket = WebSocketPending

class OAuth2ClientServiceI p => OAuth2ClientControllerI p where
    socket :: HasCallStack => Proxy p -> Context.Context -> PendingConnection -> Handler ()
    socket = socketImpl
    server :: HasCallStack => Proxy p -> Context.Context -> Server API
    server = serverImpl

socketImpl :: forall p. (HasCallStack, OAuth2ClientControllerI p) => Proxy p -> Context.Context -> PendingConnection -> Handler ()
socketImpl _ ctx socketConn =
    runPaperMonad ctx $ OAuth2.Client.Service.socket @p ctx (oauth2ClientSocketConnections ctx) socketConn (paperAuthPool ctx)

serverImpl :: (HasCallStack, OAuth2ClientControllerI p) => Proxy p -> Context.Context -> Server API
serverImpl p context =
        socket p context