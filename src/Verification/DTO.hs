{-# LANGUAGE TemplateHaskell #-}
module Verification.DTO(
    PhoneNumberSecretWrongDTO(
        PhoneNumberSecretWrongDTO
      , msg
      , failCount
      )
  , phoneNumberSecretWrongDTO
) where

import Data.Aeson
import Data.Aeson.TH

data PhoneNumberSecretWrongDTO = PhoneNumberSecretWrongDTO {
    msg :: String
  , failCount :: Int
  }
$(deriveToJSON defaultOptions ''PhoneNumberSecretWrongDTO)

phoneNumberSecretWrongDTO :: Int -> PhoneNumberSecretWrongDTO
phoneNumberSecretWrongDTO failCount = PhoneNumberSecretWrongDTO {
    msg = "phoneNumberSecret wrong"
  , failCount
  }