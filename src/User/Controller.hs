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

import JWT.Model
import User.DTO
import Authentication()
import Context
import MIME
import PaperMonad

import Servant
import Web.Cookie

import GHC.Stack

type API =
        "userInfo" :> (
                GetUserInfo
            :<|> PatchUserInfo
            )
    :<|> "enroll" :> Enroll

type GetUserInfo = AuthProtect "jwt-auth" :> Get '[PrettyJSON] GetUserInfoResDTO
type PatchUserInfo = AuthProtect "jwt-auth" :> ReqBody '[PrettyJSON] PatchUserInfoReqDTO :> Patch '[PlainText] NoContent
type Enroll = ReqBody '[PrettyJSON] EnrollReqDTO :> Post '[PrettyJSON] (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)

class UserServiceI p => UserControllerI p where
    getUserInfo :: HasCallStack => Proxy p -> Context.Context -> AuthenticatedUser -> Handler GetUserInfoResDTO
    getUserInfo = getUserInfoImpl
    patchUserInfo :: HasCallStack => Proxy p -> Context.Context -> AuthenticatedUser -> PatchUserInfoReqDTO -> Handler NoContent
    patchUserInfo = patchUserInfoImpl
    enroll :: HasCallStack => Proxy p -> Context.Context -> EnrollReqDTO -> Handler (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
    enroll = enrollImpl
    server :: HasCallStack => Proxy p -> Context.Context -> Server API
    server = serverImpl

getUserInfoImpl :: forall p. (HasCallStack, UserControllerI p) => Proxy p -> Context.Context -> AuthenticatedUser -> Handler GetUserInfoResDTO
getUserInfoImpl _ ctx (AuthenticatedUser { userId }) =
    runPaperMonad ctx $ User.Service.getUserInfo @p userId $ paperAuthPool ctx

patchUserInfoImpl :: forall p. (HasCallStack, UserControllerI p) => Proxy p -> Context.Context -> AuthenticatedUser -> PatchUserInfoReqDTO -> Handler NoContent
patchUserInfoImpl _ ctx (AuthenticatedUser { userId }) (PatchUserInfoReqDTO { paperId, password, name, phoneNumber, phoneNumberSecret }) =
    runPaperMonad ctx $ User.Service.patchUserInfo @p
        userId paperId password name phoneNumber phoneNumberSecret $ paperAuthPool ctx

enrollImpl :: forall p. (HasCallStack, UserControllerI p) => Proxy p -> Context.Context -> EnrollReqDTO -> Handler (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
enrollImpl _  ctx (EnrollReqDTO { paperId, password, phoneNumber, phoneNumberSecret }) =
    let encodeSigner = paperEncodeSigner ctx in
    runPaperMonad ctx $ User.Service.enroll @p
        (config ctx) encodeSigner paperId password phoneNumber phoneNumberSecret (paperAuthPool ctx)

serverImpl :: (HasCallStack, UserControllerI p) => Proxy p -> Context.Context -> Server API
serverImpl p ctx =
                (getUserInfo p ctx
            :<|> patchUserInfo p ctx
            )
    :<|> enroll p ctx