{-# LANGUAGE OverloadedStrings #-}

module Verification.Util(
    PhoneNumber(PhoneNumber)
  , stringToPhoneNumber
  , generatePhoneNumberSecret
) where

import PaperError
import CallStack

import Servant
import Control.Monad.IO.Unlift
import Text.Regex.TDFA
import System.Random
import GHC.Stack

newtype PhoneNumber = PhoneNumber String

stringToPhoneNumber :: (HasCallStack, MonadUnliftIO m) => String -> PaperExceptT m PhoneNumber
stringToPhoneNumber phoneNumber = do
    if phoneNumber =~ ("^[0-9]{3}-[0-9]{4}-[0-9]{4}$" :: String) :: Bool then
        return $ PhoneNumber phoneNumber
    else
        toPaperExceptT $ PaperException "phoneNumber invalid" (err400 { errBody = "phoneNumber invalid" }) callStack'

generatePhoneNumberSecret :: (HasCallStack, MonadUnliftIO m) => PaperExceptT m String
generatePhoneNumberSecret = do
    gen <- paperLiftIO newStdGen
    return $ take 6 $ randomRs ('0', '9') gen