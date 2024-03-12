module SMS.ExService(
    SMSExServiceI(
        smsNotify
      )
) where

import SMS.Profile
import Verification.Util
import PaperMonad

import Data.Configurator.Types

import Control.Monad.IO.Unlift
import GHC.Stack


class (SMSProfileC p) => SMSExServiceI p where
    smsNotify :: (HasCallStack, MonadUnliftIO m) => Config -> PhoneNumber -> String -> PaperMonad p m ()