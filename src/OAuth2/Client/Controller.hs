{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module OAuth2.Client.Controller(
    API
  , OAuth2ClientControllerI(
        server
      )
) where

import qualified OAuth2.Client.Service
import OAuth2.Client.Service(OAuth2ClientServiceI)

import Context
import Enum
import PaperMonad

import Servant
import Servant.API.WebSocket
import Network.WebSockets.Connection
import Web.Cookie

import GHC.Stack

type API =
        "webSocket" :> ConnectSocketWeb
    :<|> "issue" :> (
                "kakao" :> IssueJWT
            :<|> "naver" :> IssueJWT
            )
    :<|> "finalize" :> Finalize

type ConnectSocketWeb = WebSocketPending
type IssueJWT = QueryParam "code" String :> QueryParam "state" String :> Get '[PlainText] (Headers '[Header "Set-Cookie" SetCookie] String)
type Finalize = QueryParam "state" String :> Get '[PlainText] NoContent

class OAuth2ClientServiceI p => OAuth2ClientControllerI p where
    webSocket :: HasCallStack => Proxy p -> Context.Context -> PendingConnection -> Handler ()
    webSocket = webSocketImpl
    issueJWTKakao :: HasCallStack => Proxy p -> Context.Context -> Maybe String -> Maybe String -> Handler (Headers '[Header "Set-Cookie" SetCookie] String)
    issueJWTKakao = issueJWTKakaoImpl
    issueJWTNaver :: HasCallStack => Proxy p -> Context.Context -> Maybe String -> Maybe String -> Handler (Headers '[Header "Set-Cookie" SetCookie] String)
    issueJWTNaver = issueJWTNaverImpl
    issueJWT :: HasCallStack => Proxy p -> Context.Context -> AuthenticationType -> Maybe String -> Maybe String -> Handler (Headers '[Header "Set-Cookie" SetCookie] String)
    issueJWT = issueJWTImpl
    finalize :: HasCallStack => Proxy p -> Context.Context -> Maybe String -> Handler NoContent
    finalize = finalizeImpl
    server :: HasCallStack => Proxy p -> Context.Context -> Server API
    server = serverImpl

webSocketImpl :: forall p. (HasCallStack, OAuth2ClientControllerI p) => Proxy p -> Context.Context -> PendingConnection -> Handler ()
webSocketImpl _ ctx socketConn =
    runPaperMonad ctx $ OAuth2.Client.Service.webSocket @p ctx (oauth2ClientSocketConnections ctx) socketConn (paperAuthPool ctx)

issueJWTKakaoImpl :: (HasCallStack, OAuth2ClientControllerI p) => Proxy p -> Context.Context -> Maybe String -> Maybe String -> Handler (Headers '[Header "Set-Cookie" SetCookie] String)
issueJWTKakaoImpl p ctx = issueJWT p ctx Kakao

issueJWTNaverImpl :: (HasCallStack, OAuth2ClientControllerI p) => Proxy p -> Context.Context -> Maybe String -> Maybe String -> Handler (Headers '[Header "Set-Cookie" SetCookie] String)
issueJWTNaverImpl p ctx = issueJWT p ctx Naver

issueJWTImpl :: forall p. (HasCallStack, OAuth2ClientControllerI p) => Proxy p -> Context.Context -> AuthenticationType -> Maybe String -> Maybe String -> Handler (Headers '[Header "Set-Cookie" SetCookie] String)
issueJWTImpl _ _ _ Nothing _ =
    throwError $ err400 { errBody = "missing code" }
issueJWTImpl _ _ _ _ Nothing =
    throwError $ err400 { errBody = "missing state" }
issueJWTImpl _ ctx authenticationType (Just code) (Just state) =
    runPaperMonad ctx $ OAuth2.Client.Service.issueJWT @p (config ctx) (paperEncodeSigner ctx) authenticationType code state (paperAuthPool ctx)

finalizeImpl :: forall p. (HasCallStack, OAuth2ClientControllerI p) => Proxy p -> Context.Context -> Maybe String -> Handler NoContent
finalizeImpl _ _ Nothing =
    throwError $ err400 { errBody = "missing state" }
finalizeImpl _ ctx (Just state) =
    runPaperMonad ctx $ OAuth2.Client.Service.finalize @p state (paperAuthPool ctx)

serverImpl :: (HasCallStack, OAuth2ClientControllerI p) => Proxy p -> Context.Context -> Server API
serverImpl p ctx =
        webSocket p ctx
    :<|> (
                issueJWTKakao p ctx
            :<|> issueJWTNaver p ctx
            )
    :<|> finalize p ctx