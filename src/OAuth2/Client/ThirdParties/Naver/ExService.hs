module OAuth2.Client.ThirdParties.Naver.ExService(
    OAuth2ClientNaverExServiceI(
        getIdentifier
      )
) where

import PaperMonad

import Data.Configurator.Types

import Control.Monad.IO.Unlift
import GHC.Stack

class PaperMonadI p => OAuth2ClientNaverExServiceI p where
    getIdentifier :: (HasCallStack, MonadUnliftIO m) => Config -> String -> String -> PaperMonad p m String
    getIdentifier = getIdentifierImpl

getIdentifierImpl :: (HasCallStack, OAuth2ClientNaverExServiceI p, MonadUnliftIO m) => Config -> String -> String -> PaperMonad p m String
getIdentifierImpl cfg code state = return $ "NAVER" ++ code ++ state