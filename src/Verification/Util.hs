{-# LANGUAGE OverloadedStrings #-}

module Verification.Util(
    PhoneNumber(PhoneNumber)
  , VerificationUtilI(
        stringToPhoneNumber
      , generatePhoneNumberSecret
      )
) where

import PaperMonad
import CallStack

import Servant

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Text.Regex.TDFA
import System.Random
import GHC.Stack

newtype PhoneNumber = PhoneNumber String

class PaperMonadI p => VerificationUtilI p where
    stringToPhoneNumber :: (HasCallStack, Monad m) => String -> PaperMonad p m PhoneNumber
    stringToPhoneNumber = stringToPhoneNumberImpl
    generatePhoneNumberSecret :: (HasCallStack, MonadUnliftIO m) => PaperMonad p m String
    generatePhoneNumberSecret = generatePhoneNumberSecretImpl

stringToPhoneNumberImpl :: (HasCallStack, VerificationUtilI p, Monad m) => String -> PaperMonad p m PhoneNumber
stringToPhoneNumberImpl phoneNumber = do
    profile <- ask
    if phoneNumber =~ ("^[0-9]{3}-[0-9]{4}-[0-9]{4}$" :: String) :: Bool then
        return $ PhoneNumber phoneNumber
    else
        toPaperMonad $ PaperError "phoneNumber invalid" (err400 { errBody = "phoneNumber invalid" }) (callStack' profile)

generatePhoneNumberSecretImpl :: (HasCallStack, VerificationUtilI p, MonadUnliftIO m) => PaperMonad p m String
generatePhoneNumberSecretImpl = do
    gen <- paperLiftIOUnliftIO newStdGen
    return $ take 6 $ randomRs ('0', '9') gen