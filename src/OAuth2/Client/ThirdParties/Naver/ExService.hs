{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module OAuth2.Client.ThirdParties.Naver.ExService(
    OAuth2ClientNaverExServiceI(
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
import Data.Aeson.TH
import Data.Configurator.Types

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import GHC.Stack

data GetTokenResDTO = GetTokenResDTO {
    access_token :: String
  } deriving Show
$(deriveJSON defaultOptions ''GetTokenResDTO)

data ResponseDTO = ResponseDTO {
    id :: String
  } deriving Show
$(deriveJSON defaultOptions ''ResponseDTO)

data GetIdentifierResDTO = GetIdentifierResDTO {
    response :: ResponseDTO
  } deriving Show
$(deriveJSON defaultOptions ''GetIdentifierResDTO)

type GetTokenC = "oauth2.0" :> "token" :>
    QueryParam "grant_type" String :> QueryParam "client_id" String :> QueryParam "client_secret" String :> QueryParam "code" String :> QueryParam "state" String :>
    Get '[JSON] GetTokenResDTO

type GetIdentifierC = "v1" :> "nid" :> "me" :>
    Header "Authorization" String :>
    Get '[JSON] GetIdentifierResDTO

getTokenC :: Client ClientM GetTokenC
getTokenC = client (Proxy :: Proxy GetTokenC)

getIdentifierC :: Client ClientM GetIdentifierC
getIdentifierC = client (Proxy :: Proxy GetIdentifierC)

class (ConfiguratorI p, PaperMonadI p) => OAuth2ClientNaverExServiceI p where
    getIdentifier :: (HasCallStack, MonadUnliftIO m) => Config -> String -> String -> PaperMonad p m String
    getIdentifier = getIdentifierImpl

getIdentifierImpl :: (HasCallStack, OAuth2ClientNaverExServiceI p, MonadUnliftIO m) => Config -> String -> String -> PaperMonad p m String
getIdentifierImpl cfg code state = do
    profile <- ask
    clientId <- lookupRequired cfg "oauth2-client.naver.client-id"
    clientSecret <- lookupRequired cfg "oauth2-client.naver.client-secret"
    manager <- paperLiftIOUnliftIO $ newTlsManager
    baseUrl' <- paperLiftIOUnliftIO $ parseBaseUrl "https://nid.naver.com"
    let clientEnv = mkClientEnv manager baseUrl'
    result <- paperLiftIOUnliftIO $ runClientM (getTokenC
        (Just "authorization_code") (Just clientId) (Just clientSecret) (Just code) (Just state)
        ) clientEnv
    case result of
        Left err -> toPaperMonad $ PaperCatchError err (err500 { errBody = "naver social login connection error" }) $ callStack' profile
        Right (GetTokenResDTO {
            access_token
          }) -> do
            baseUrl'' <- paperLiftIOUnliftIO $ parseBaseUrl "https://openapi.naver.com"
            let clientEnv' = mkClientEnv manager baseUrl''
            let bearerToken = "Bearer " ++ access_token
            result' <- paperLiftIOUnliftIO $ runClientM (getIdentifierC (Just bearerToken)) clientEnv'
            case result' of
                Left err -> toPaperMonad $ PaperCatchError err (err500 { errBody = "naver social login connection error" }) $ callStack' profile
                Right (GetIdentifierResDTO {
                    response = ResponseDTO {
                        id
                      }
                  }) -> return id