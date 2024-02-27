{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module JWT.Controller(
    API
  , server
) where

import qualified JWT.Service

import JWT.Model
import JWT.DTO
import Context
import PaperError
import Configurator

import Servant
import Servant.Auth.Server
import Web.Cookie
import GHC.Stack

type API = IssueJWT
    :<|> "indirect" :> (
        "request" :> IndirectRequestJWT
        :<|> "issue" :> IndirectIssueJWT
        )

type IssueJWT = ReqBody '[JSON] IssueJWTReqDTO :> Post '[JSON] IssueJWTResDTO
type IndirectRequestJWT = Get '[JSON] NoContent
type IndirectIssueJWT = Get '[JSON] NoContent
type InvalidateJWT = Delete '[PlainText] NoContent
type RefreshJWT = "refresh" :> Get '[JSON] NoContent

issueJWT :: HasCallStack => Context.Context -> IssueJWTReqDTO -> Handler IssueJWTResDTO
issueJWT context (IssueJWTReqDTO { paperId, password }) = do
    let encodeSigner = paperEncodeSigner context
    runPaperExceptT $ JWT.Service.issueJWT
        (config context) (paperAuthPool context) encodeSigner paperId password

server :: HasCallStack => Context.Context -> Server API
server context = issueJWT context
    :<|> undefined