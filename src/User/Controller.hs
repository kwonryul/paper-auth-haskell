{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module User.Controller(
    API
  , UserControllerI(
        server
      )
) where

import qualified User.Service
import User.Service(UserServiceI)

import Authentication ()
import User.DTO
import Context
import PaperMonad

import Servant
import Web.Cookie

import GHC.Stack

type API =
    "verify" :> (
        VerifyRequest
        :<|> VerifyCheck
        )
    :<|> Enroll

type VerifyRequest = "request" :> ReqBody '[JSON] VerifyRequestReqDTO :> Post '[PlainText] NoContent
type VerifyCheck = "check" :> ReqBody '[JSON] VerifyCheckReqDTO :> Post '[JSON] VerifyCheckResDTO
type Enroll = "enroll" :> ReqBody '[JSON] EnrollReqDTO :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)

class UserServiceI p => UserControllerI p where
    verifyRequest :: HasCallStack => Proxy p -> Context.Context -> VerifyRequestReqDTO -> Handler NoContent
    verifyRequest = verifyRequestImpl
    verifyCheck :: HasCallStack => Proxy p -> Context.Context -> VerifyCheckReqDTO -> Handler VerifyCheckResDTO
    verifyCheck = verifyCheckImpl
    enroll :: HasCallStack => Proxy p -> Context.Context -> EnrollReqDTO -> Handler (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
    enroll = enrollImpl
    server :: HasCallStack => Proxy p -> Context.Context -> Server API
    server = serverImpl

verifyRequestImpl :: forall p. (HasCallStack, UserControllerI p) => Proxy p -> Context.Context -> VerifyRequestReqDTO -> Handler NoContent
verifyRequestImpl _ context (VerifyRequestReqDTO { phoneNumber }) =
    runPaperMonad context $ User.Service.verifyRequest @p
        phoneNumber (paperAuthPool context)

verifyCheckImpl :: forall p. (HasCallStack, UserControllerI p) => Proxy p -> Context.Context -> VerifyCheckReqDTO -> Handler VerifyCheckResDTO
verifyCheckImpl _ context (VerifyCheckReqDTO { phoneNumber, phoneNumberSecret }) =
    runPaperMonad context $ User.Service.verifyCheck @p
        phoneNumber phoneNumberSecret (paperAuthPool context)

enrollImpl :: forall p. (HasCallStack, UserControllerI p) => Proxy p -> Context.Context -> EnrollReqDTO -> Handler (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
enrollImpl _  context (EnrollReqDTO { paperId, password, name, phoneNumber, phoneNumberSecret }) =
    let encodeSigner = paperEncodeSigner context in
    runPaperMonad context $ User.Service.enroll @p
        (config context) encodeSigner paperId password name phoneNumber phoneNumberSecret (paperAuthPool context)

serverImpl :: (HasCallStack, UserControllerI p) => Proxy p -> Context.Context -> Server API
serverImpl p context = (
        verifyRequest p context
        :<|> verifyCheck p context
        )
    :<|> enroll p context