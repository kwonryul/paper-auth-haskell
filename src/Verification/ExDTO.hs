{-# LANGUAGE TemplateHaskell #-}
module Verification.ExDTO(
    PhoneNumberSecretWrongDTO(
        PhoneNumberSecretWrongDTO
      , msg
      , failCount
      )
  , VerificationExDTOI(
        phoneNumberSecretWrongDTO
      )
) where

import Monad.ProfileT
import Definition

import Data.Aeson
import Data.Aeson.TH

import Data.Proxy

$(defineDTO "verification-ex/phoneNumberSecretWrongDTO.dto")
$(deriveJSON defaultOptions ''PhoneNumberSecretWrongDTO)

class Profile p => VerificationExDTOI p where
    phoneNumberSecretWrongDTO :: Proxy p -> Int -> PhoneNumberSecretWrongDTO
    phoneNumberSecretWrongDTO = phoneNumberSecretWrongDTOImpl

phoneNumberSecretWrongDTOImpl :: VerificationExDTOI p => Proxy p -> Int -> PhoneNumberSecretWrongDTO
phoneNumberSecretWrongDTOImpl _ failCount = PhoneNumberSecretWrongDTO {
    msg = "phoneNumberSecret wrong"
  , failCount
  }