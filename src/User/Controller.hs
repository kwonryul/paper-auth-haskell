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

import Authentication()
import User.DTO
import Context
import PaperMonad
import MIME

import Servant
import Web.Cookie

import GHC.Stack

type API =
         "enroll" :> Enroll

type Enroll = ReqBody '[PrettyJSON] EnrollReqDTO :> Post '[PrettyJSON] (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)

class UserServiceI p => UserControllerI p where
    enroll :: HasCallStack => Proxy p -> Context.Context -> EnrollReqDTO -> Handler (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
    enroll = enrollImpl
    server :: HasCallStack => Proxy p -> Context.Context -> Server API
    server = serverImpl

enrollImpl :: forall p. (HasCallStack, UserControllerI p) => Proxy p -> Context.Context -> EnrollReqDTO -> Handler (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
enrollImpl _  context (EnrollReqDTO { paperId, password, name, phoneNumber, phoneNumberSecret }) =
    let encodeSigner = paperEncodeSigner context in
    runPaperMonad context $ User.Service.enroll @p
        (config context) encodeSigner paperId password name phoneNumber phoneNumberSecret (paperAuthPool context)

serverImpl :: (HasCallStack, UserControllerI p) => Proxy p -> Context.Context -> Server API
serverImpl p context =
        enroll p context