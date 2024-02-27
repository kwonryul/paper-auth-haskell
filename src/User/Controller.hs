{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module User.Controller(
    API
  , server
) where

import qualified User.Service

import User.DTO
import Context
import PaperError

import Servant

import GHC.Stack

{-
type API = "user" :> (
    Enroll
    :<|> Login
    :<|> RefreshToken
    :<|> Withdrawal
    :<|> "info" :> (
            GetUserInfo
            :<|> PatchUserInfo
        )
    )
-}

type API =
    "verify" :> (
        VerifyRequest
        :<|> VerifyCheck
        )
    :<|> Enroll

type VerifyRequest = "request" :> ReqBody '[JSON] VerifyRequestReqDTO :> Post '[PlainText] NoContent
type VerifyCheck = "check" :> ReqBody '[JSON] VerifyCheckReqDTO :> Post '[JSON] Bool
type Enroll = "enroll" :> ReqBody '[JSON] EnrollReqDTO :> Post '[JSON] EnrollResDTO

verifyRequest :: HasCallStack => Context.Context -> VerifyRequestReqDTO -> Handler NoContent
verifyRequest context (VerifyRequestReqDTO { phoneNumber }) = do
    runPaperExceptT $ User.Service.verifyRequest
        (paperAuthPool context) phoneNumber

verifyCheck :: HasCallStack => Context.Context -> VerifyCheckReqDTO -> Handler Bool
verifyCheck context (VerifyCheckReqDTO { phoneNumber, phoneNumberSecret }) = do
    runPaperExceptT $ User.Service.verifyCheck
        (paperAuthPool context) phoneNumber phoneNumberSecret

enroll :: HasCallStack => Context.Context -> EnrollReqDTO -> Handler EnrollResDTO
enroll context (EnrollReqDTO { paperId, password, name, phoneNumber, phoneNumberSecret }) = do
    let encodeSigner = paperEncodeSigner context
    runPaperExceptT $ User.Service.enroll
        (config context) (paperAuthPool context) encodeSigner paperId password name phoneNumber phoneNumberSecret

server :: HasCallStack => Context.Context -> Server API
server context = (
        verifyRequest context
        :<|> verifyCheck context
        )
    :<|> enroll context