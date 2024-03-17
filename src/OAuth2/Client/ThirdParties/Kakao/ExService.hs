module OAuth2.Client.ThirdParties.Kakao.ExService(
    OAuth2ClientKakaoExServiceI(
        getIdentifier
      )
) where

import PaperMonad

import Data.Configurator.Types

import Control.Monad.IO.Unlift
import GHC.Stack

class PaperMonadI p => OAuth2ClientKakaoExServiceI p where
    getIdentifier :: (HasCallStack, MonadUnliftIO m) => Config -> String -> String -> PaperMonad p m String
    getIdentifier = getIdentifierImpl

getIdentifierImpl :: (HasCallStack, OAuth2ClientKakaoExServiceI p, MonadUnliftIO m) => Config -> String -> String -> PaperMonad p m String
getIdentifierImpl cfg code state = return $ "KAKAO" ++ code ++ state
