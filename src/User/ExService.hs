{-# LANGUAGE OverloadedStrings #-}

module User.ExService(
    UserExServiceI(
        hashPassword
      )
) where

import CallStack
import PaperMonad

import Servant
import Crypto.BCrypt

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ByteString.Char8
import GHC.Stack

class PaperMonadI p => UserExServiceI p where
    hashPassword :: (HasCallStack, MonadUnliftIO m) => String -> PaperMonad p m ByteString
    hashPassword = hashPasswordImpl

hashPasswordImpl :: (HasCallStack, UserExServiceI p, MonadUnliftIO m) => String -> PaperMonad p m ByteString
hashPasswordImpl password = do
    profile <- ask
    maybeTToPaperMonadUnliftIO
        (MaybeT $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (Data.ByteString.Char8.pack password))
        $ PaperError "hashing string error" (err500 { errBody = "internal server error" }) (callStack' profile)