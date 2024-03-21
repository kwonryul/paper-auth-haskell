{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module OAuth2.Client.ThirdParties.Kakao.ExService(
    OAuth2ClientKakaoExServiceI(
        getIdentifier
      )
) where

import Prelude hiding (id)

import CallStack
import Configurator
import PaperMonad

import Servant
import Servant.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Media
import Data.Aeson.TH
import Data.Configurator.Types

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8
import GHC.Stack

data FormUrlEncoded'

instance Servant.Accept FormUrlEncoded' where
    contentType _ = "application" Network.HTTP.Media.// "x-www-form-urlencoded" Network.HTTP.Media./: ("charset", "utf-8")

instance MimeRender FormUrlEncoded' String where
    mimeRender _ = Data.ByteString.Lazy.Char8.pack

data GetTokenResDTO = GetTokenResDTO {
    access_token :: String
  } deriving Show
$(deriveJSON defaultOptions ''GetTokenResDTO)

data GetIdentifierResDTO = GetIdentifierResDTO {
    id :: Int
  } deriving Show
$(deriveJSON defaultOptions ''GetIdentifierResDTO)

type GetTokenC = "oauth" :> "token" :> ReqBody '[FormUrlEncoded'] String :> Post '[JSON] GetTokenResDTO

type GetIdentifierC = "v1" :> "user" :> "access_token_info" :> Header "Authorization" String :> Get '[JSON] GetIdentifierResDTO

getTokenC :: Client ClientM GetTokenC
getTokenC = client (Servant.Proxy :: Servant.Proxy GetTokenC)

getIdentifierC :: Client ClientM GetIdentifierC
getIdentifierC = client (Servant.Proxy :: Servant.Proxy GetIdentifierC)

class (ConfiguratorI p, PaperMonadI p) => OAuth2ClientKakaoExServiceI p where
    getIdentifier :: (HasCallStack, MonadUnliftIO m) => Config -> String -> PaperMonad p m String
    getIdentifier = getIdentifierImpl

getIdentifierImpl :: (HasCallStack, OAuth2ClientKakaoExServiceI p, MonadUnliftIO m) => Config -> String -> PaperMonad p m String
getIdentifierImpl cfg code = do
    profile <- ask
    clientId <- lookupRequired cfg "oauth2-client.kakao.client-id"
    clientSecret <- lookupRequired cfg "oauth2-client.kakao.client-secret"
    redirectUri <- lookupRequired cfg "oauth2-client.kakao.redirect-uri"
    let body = "grant_type=authorization_code&client_id=" ++ clientId ++ "&redirect_uri=" ++ redirectUri ++
            "&code=" ++ code ++ "&client_secret=" ++ clientSecret
    baseUrl' <- paperLiftIOUnliftIO $ parseBaseUrl "https://kauth.kakao.com"
    manager <- paperLiftIOUnliftIO $ newTlsManager
    let clientEnv = mkClientEnv manager baseUrl'
    result <- paperLiftIOUnliftIO $ runClientM (getTokenC body) clientEnv
    case result of
        Left err -> toPaperMonad $ PaperCatchError err (err500 { errBody = "kakao social login connection error" }) $ callStack' profile
        Right (GetTokenResDTO {
            access_token
          })-> do
            baseUrl'' <- paperLiftIOUnliftIO $ parseBaseUrl "https://kapi.kakao.com"
            let clientEnv' = mkClientEnv manager baseUrl''
            let bearerToken = "Bearer " ++ access_token
            result' <- paperLiftIOUnliftIO $ runClientM (getIdentifierC (Just bearerToken)) clientEnv'
            case result' of
                Left err' -> toPaperMonad $ PaperCatchError err' (err500 { errBody = "kakao social login connection error" }) $ callStack' profile
                Right (GetIdentifierResDTO {
                    id
                  }) -> return $ show id