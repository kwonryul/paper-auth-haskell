{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module JWT.Controller(
    JWTControllerI(
        server
      )
  , API
) where

import qualified JWT.Service
import JWT.Service (JWTServiceI)

import Authentication()
import JWT.Model
import JWT.DTO
import Context
import PaperMonad
import MIME

import Servant
import Web.Cookie

import GHC.Stack

type API =
        "issue" :> IssueJWT
    :<|> "refresh" :> RefreshJWT
    :<|> "invalidate" :> InvalidateJWT

type IssueJWT = ReqBody '[PrettyJSON] IssueJWTReqDTO :> Post '[PrettyJSON] (Headers '[Header "Set-Cookie" SetCookie] IssueJWTResDTO)
type RefreshJWT = AuthProtect "jwt-auth-refresh" :> Post '[PrettyJSON] (Headers '[Header "Set-Cookie" SetCookie] RefreshJWTResDTO)
type InvalidateJWT = AuthProtect "jwt-auth" :> Delete '[PlainText] NoContent

class JWTServiceI p => JWTControllerI p where
    issueJWT :: HasCallStack => Proxy p -> Context.Context -> IssueJWTReqDTO -> Handler (Headers '[Header "Set-Cookie" SetCookie] IssueJWTResDTO)
    issueJWT = issueJWTImpl
    refreshJWT :: HasCallStack => Proxy p -> Context.Context -> AuthenticatedUserRefresh -> Handler (Headers '[Header "Set-Cookie" SetCookie] RefreshJWTResDTO)
    refreshJWT = refreshJWTImpl
    invalidateJWT :: HasCallStack => Proxy p -> Context.Context -> AuthenticatedUser -> Handler NoContent
    invalidateJWT = invalidateJWTImpl
    server :: HasCallStack => Proxy p -> Context.Context -> Server API
    server = serverImpl

issueJWTImpl :: forall p. (HasCallStack, JWTControllerI p) => Proxy p -> Context.Context -> IssueJWTReqDTO -> Handler (Headers '[Header "Set-Cookie" SetCookie] IssueJWTResDTO)
issueJWTImpl _ context (IssueJWTReqDTO { paperId, password }) = do
    let encodeSigner = paperEncodeSigner context
    runPaperMonad context $ JWT.Service.issueJWT @p (config context) encodeSigner paperId password (paperAuthPool context)


refreshJWTImpl :: forall p. (HasCallStack, JWTControllerI p) => Proxy p -> Context.Context -> AuthenticatedUserRefresh -> Handler (Headers '[Header "Set-Cookie" SetCookie] RefreshJWTResDTO)
refreshJWTImpl _ context (AuthenticatedUserRefresh { userId }) = do
    let encodeSigner = paperEncodeSigner context
    runPaperMonad context $ JWT.Service.refreshJWT @p
        (config context) encodeSigner userId (paperAuthPool context)

invalidateJWTImpl :: forall p. (HasCallStack, JWTControllerI p) => Proxy p -> Context.Context -> AuthenticatedUser -> Handler NoContent
invalidateJWTImpl _ context (AuthenticatedUser { userId }) = do
    runPaperMonad context $ JWT.Service.invalidateJWT @p
        userId (paperAuthPool context)

serverImpl :: (HasCallStack, JWTControllerI p) => Proxy p -> Context.Context -> Server API
serverImpl p context =
        issueJWT p context
    :<|> refreshJWT p context
    :<|> invalidateJWT p context