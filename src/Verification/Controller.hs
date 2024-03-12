{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Verification.Controller(
    API
  , VerificationControllerI(
        server
      )
) where

import qualified Verification.Service
import Verification.Service(VerificationServiceI)

import Verification.DTO
import Authentication ()
import Context
import MIME
import PaperMonad

import Servant

import GHC.Stack

type API =
        "request" :> VerifyRequest
    :<|> "check" :> VerifyCheck

type VerifyRequest = ReqBody '[PrettyJSON] VerifyRequestReqDTO :> Post '[PlainText] NoContent
type VerifyCheck = ReqBody '[PrettyJSON] VerifyCheckReqDTO :> Post '[PrettyJSON] VerifyCheckResDTO

class VerificationServiceI p => VerificationControllerI p where
    verifyRequest :: HasCallStack => Proxy p -> Context.Context -> VerifyRequestReqDTO -> Handler NoContent
    verifyRequest = verifyRequestImpl
    verifyCheck :: HasCallStack => Proxy p -> Context.Context -> VerifyCheckReqDTO -> Handler VerifyCheckResDTO
    verifyCheck = verifyCheckImpl
    server :: HasCallStack => Proxy p -> Context.Context -> Server API
    server = serverImpl

verifyRequestImpl :: forall p. (HasCallStack, VerificationControllerI p) => Proxy p -> Context.Context -> VerifyRequestReqDTO -> Handler NoContent
verifyRequestImpl _ context (VerifyRequestReqDTO { phoneNumber }) =
    runPaperMonad context $ Verification.Service.verifyRequest @p
        (config context) phoneNumber (paperAuthPool context)

verifyCheckImpl :: forall p. (HasCallStack, VerificationControllerI p) => Proxy p -> Context.Context -> VerifyCheckReqDTO -> Handler VerifyCheckResDTO
verifyCheckImpl _ context (VerifyCheckReqDTO { phoneNumber, phoneNumberSecret }) =
    runPaperMonad context $ Verification.Service.verifyCheck @p
        phoneNumber phoneNumberSecret (paperAuthPool context)

serverImpl :: (HasCallStack, VerificationControllerI p) => Proxy p -> Context.Context -> Server API
serverImpl p context =
        verifyRequest p context
    :<|> verifyCheck p context