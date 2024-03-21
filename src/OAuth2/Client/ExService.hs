{-# LANGUAGE OverloadedStrings #-}

module OAuth2.Client.ExService(
    OAuth2ClientExServiceI(
        getIdentifier
      )
) where

import qualified OAuth2.Client.ThirdParties.Kakao.ExService
import OAuth2.Client.ThirdParties.Kakao.ExService(OAuth2ClientKakaoExServiceI)
import qualified OAuth2.Client.ThirdParties.Naver.ExService
import OAuth2.Client.ThirdParties.Naver.ExService(OAuth2ClientNaverExServiceI)

import CallStack
import Enum
import PaperMonad

import Servant
import Data.Configurator.Types

import Control.Monad.IO.Unlift
import GHC.Stack

class (OAuth2ClientKakaoExServiceI p, OAuth2ClientNaverExServiceI p) => OAuth2ClientExServiceI p where
    getIdentifier :: (HasCallStack, MonadUnliftIO m) => Config -> AuthenticationType -> String -> String -> PaperMonad p m String
    getIdentifier = getIdentifierImpl

getIdentifierImpl :: forall p m. (HasCallStack, OAuth2ClientExServiceI p, MonadUnliftIO m) => Config -> AuthenticationType -> String -> String -> PaperMonad p m String
getIdentifierImpl cfg Kakao code _ = OAuth2.Client.ThirdParties.Kakao.ExService.getIdentifier cfg code
getIdentifierImpl cfg Naver code state = OAuth2.Client.ThirdParties.Naver.ExService.getIdentifier cfg code state
getIdentifierImpl _ _ _ _ = toPaperMonad $ PaperError "invalid authentication type" (err500 { errBody = "internal server error" }) $ callStack' profile
    where
        profile :: Proxy p
        profile = Proxy