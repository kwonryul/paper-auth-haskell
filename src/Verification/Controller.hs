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
verifyRequestImpl _ ctx (VerifyRequestReqDTO { phoneNumber }) =
    runPaperMonad (config ctx) $ Verification.Service.verifyRequest @p
        (config ctx) phoneNumber (paperAuthPool ctx)

verifyCheckImpl :: forall p. (HasCallStack, VerificationControllerI p) => Proxy p -> Context.Context -> VerifyCheckReqDTO -> Handler VerifyCheckResDTO
verifyCheckImpl _ ctx (VerifyCheckReqDTO { phoneNumber, phoneNumberSecret }) =
    runPaperMonad (config ctx) $ Verification.Service.verifyCheck @p
        phoneNumber phoneNumberSecret (paperAuthPool ctx)

serverImpl :: (HasCallStack, VerificationControllerI p) => Proxy p -> Context.Context -> Server API
serverImpl p context =
        verifyRequest p context
    :<|> verifyCheck p context