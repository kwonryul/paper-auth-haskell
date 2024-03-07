{-# LANGUAGE TemplateHaskell #-}
module Verification.DTO(
    PhoneNumberSecretWrongDTO(
        PhoneNumberSecretWrongDTO
      , msg
      , failCount
      )
  , VerificationDTOI(
        phoneNumberSecretWrongDTO
      )
) where

import Monad.ProfileT

import Data.Aeson
import Data.Aeson.TH

import Data.Proxy

data PhoneNumberSecretWrongDTO = PhoneNumberSecretWrongDTO {
    msg :: String
  , failCount :: Int
  }
$(deriveJSON defaultOptions ''PhoneNumberSecretWrongDTO)

class Profile p => VerificationDTOI p where
    phoneNumberSecretWrongDTO :: Proxy p -> Int -> PhoneNumberSecretWrongDTO
    phoneNumberSecretWrongDTO = phoneNumberSecretWrongDTOImpl

phoneNumberSecretWrongDTOImpl :: VerificationDTOI p => Proxy p -> Int -> PhoneNumberSecretWrongDTO
phoneNumberSecretWrongDTOImpl _ failCount = PhoneNumberSecretWrongDTO {
    msg = "phoneNumberSecret wrong"
  , failCount
  }