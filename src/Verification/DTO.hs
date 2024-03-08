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
import Definition

import Data.Aeson
import Data.Aeson.TH

import Data.Proxy

$(defineDTO "verification/phoneNumberSecretWrongDTO.dto")
$(deriveJSON defaultOptions ''PhoneNumberSecretWrongDTO)

class Profile p => VerificationDTOI p where
    phoneNumberSecretWrongDTO :: Proxy p -> Int -> PhoneNumberSecretWrongDTO
    phoneNumberSecretWrongDTO = phoneNumberSecretWrongDTOImpl

phoneNumberSecretWrongDTOImpl :: VerificationDTOI p => Proxy p -> Int -> PhoneNumberSecretWrongDTO
phoneNumberSecretWrongDTOImpl _ failCount = PhoneNumberSecretWrongDTO {
    msg = "phoneNumberSecret wrong"
  , failCount
  }