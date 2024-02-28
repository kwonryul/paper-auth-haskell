{-# LANGUAGE TemplateHaskell #-}
module Verification.DTO(
    PhoneNumberSecretWrongDTO(
        PhoneNumberSecretWrongDTO
      , msg
      , failCount
      )
) where

import Data.Aeson
import Data.Aeson.TH

data PhoneNumberSecretWrongDTO = PhoneNumberSecretWrongDTO {
    msg :: String
  , failCount :: Int
  }
$(deriveToJSON defaultOptions ''PhoneNumberSecretWrongDTO)