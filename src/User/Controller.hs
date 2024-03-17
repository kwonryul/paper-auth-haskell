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
enrollImpl _  ctx (EnrollReqDTO { paperId, password, phoneNumber, phoneNumberSecret }) =
    let encodeSigner = paperEncodeSigner ctx in
    runPaperMonad ctx $ User.Service.enroll @p
        (config ctx) encodeSigner paperId password phoneNumber phoneNumberSecret (paperAuthPool ctx)

serverImpl :: (HasCallStack, UserControllerI p) => Proxy p -> Context.Context -> Server API
serverImpl p context =
        enroll p context