{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module User.DTO(
    VerifyRequestReqDTO(
        VerifyRequestReqDTO
      , phoneNumber
      )
  , VerifyCheckReqDTO(
        VerifyCheckReqDTO
      , phoneNumber
      , phoneNumberSecret
      )
  , VerifyCheckResDTO(
        VerifyCheckResDTO
      , result
      , failCount
      )
  , EnrollReqDTO(
        EnrollReqDTO
      , paperId
      , password
      , name
      , phoneNumber
      , phoneNumberSecret
      )
  , EnrollResDTO(
        EnrollResDTO
      , accessToken
      )
) where

import Data.Aeson
import Data.Aeson.TH

import Data.Text

data VerifyRequestReqDTO = VerifyRequestReqDTO {
    phoneNumber :: String
    }
$(deriveFromJSON defaultOptions ''VerifyRequestReqDTO)

data VerifyCheckReqDTO = VerifyCheckReqDTO {
    phoneNumber :: String
  , phoneNumberSecret :: String
  }
$(deriveFromJSON defaultOptions ''VerifyCheckReqDTO)

data VerifyCheckResDTO = VerifyCheckResDTO {
    result :: Bool
  , failCount :: Int
  }
$(deriveToJSON defaultOptions ''VerifyCheckResDTO)

data EnrollReqDTO = EnrollReqDTO {
    paperId :: String
  , password :: String
  , name :: String
  , phoneNumber :: String
  , phoneNumberSecret :: String
  }
$(deriveFromJSON defaultOptions ''EnrollReqDTO)

data EnrollResDTO = EnrollResDTO {
    accessToken :: Text
  }
$(deriveToJSON defaultOptions ''EnrollResDTO)