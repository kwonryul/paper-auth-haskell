{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE CPP #-}

module JWT.Controller(
    JWTControllerI(
        server
      )
  , API
#ifdef TEST
  , API2
  , IssueJWTReqDTO
  , IssueJWTResDTO
#endif
) where

import qualified JWT.Service
import JWT.Service (JWTServiceI)

import Authentication ()
import JWT.Model
import JWT.DTO
import Context
import PaperMonad

import Servant
import Web.Cookie

import GHC.Stack

type API = IssueJWT
    :<|> RefreshJWT
    :<|> InvalidateJWT
    :<|> "indirect" :> (
        IndirectRequestJWT
        :<|> IndirectIssueJWT
        )

type API2 = IssueJWT

type IssueJWT = "issue" :> ReqBody '[JSON] IssueJWTReqDTO :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] IssueJWTResDTO)
type RefreshJWT = "refresh" :> AuthProtect "jwt-auth-refresh" :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] RefreshJWTResDTO)
type InvalidateJWT = "invalidate" :> AuthProtect "jwt-auth" :> Delete '[PlainText] NoContent
type IndirectRequestJWT = "request" :> Get '[JSON] NoContent
type IndirectIssueJWT = "issue" :> Get '[JSON] NoContent

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
serverImpl p context = issueJWT p context
    :<|> refreshJWT p context
    :<|> invalidateJWT p context
    :<|> (
        undefined
        :<|> undefined
        )