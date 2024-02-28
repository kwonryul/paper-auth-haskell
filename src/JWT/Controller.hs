{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module JWT.Controller(
    API
  , server
) where

import qualified JWT.Service

import Authentication ()
import JWT.Model
import JWT.DTO
import Context
import PaperError

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

type IssueJWT = "issue" :> ReqBody '[JSON] IssueJWTReqDTO :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] IssueJWTResDTO)
type RefreshJWT = "refresh" :> AuthProtect "jwt-auth-refresh" :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] RefreshJWTResDTO)
type InvalidateJWT = "invalidate" :> AuthProtect "jwt-auth" :> Delete '[PlainText] NoContent
type IndirectRequestJWT = "request" :> Get '[JSON] NoContent
type IndirectIssueJWT = "issue" :> Get '[JSON] NoContent

issueJWT :: HasCallStack => Context.Context -> IssueJWTReqDTO -> Handler (Headers '[Header "Set-Cookie" SetCookie] IssueJWTResDTO)
issueJWT context (IssueJWTReqDTO { paperId, password }) = do
    let encodeSigner = paperEncodeSigner context
    runPaperExceptT $ JWT.Service.issueJWT
        (config context) (paperAuthPool context) encodeSigner paperId password

refreshJWT :: HasCallStack => Context.Context -> AuthenticatedUserRefresh -> Handler (Headers '[Header "Set-Cookie" SetCookie] RefreshJWTResDTO)
refreshJWT context (AuthenticatedUserRefresh { userId }) = do
    let encodeSigner = paperEncodeSigner context
    runPaperExceptT $ JWT.Service.refreshJWT
        (config context) (paperAuthPool context) encodeSigner userId

invalidateJWT :: HasCallStack => Context.Context -> AuthenticatedUser -> Handler NoContent
invalidateJWT context (AuthenticatedUser { userId }) = do
    runPaperExceptT $ JWT.Service.invalidateJWT
        (paperAuthPool context) userId

server :: HasCallStack => Context.Context -> Server API
server context = issueJWT context
    :<|> refreshJWT context
    :<|> invalidateJWT context
    :<|> (
        undefined
        :<|> undefined
    )