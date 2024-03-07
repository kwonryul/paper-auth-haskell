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
$(deriveJSON defaultOptions ''VerifyRequestReqDTO)

data VerifyCheckReqDTO = VerifyCheckReqDTO {
    phoneNumber :: String
  , phoneNumberSecret :: String
  }
$(deriveJSON defaultOptions ''VerifyCheckReqDTO)

data VerifyCheckResDTO = VerifyCheckResDTO {
    result :: Bool
  , failCount :: Int
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''VerifyCheckResDTO)

data EnrollReqDTO = EnrollReqDTO {
    paperId :: String
  , password :: String
  , name :: String
  , phoneNumber :: String
  , phoneNumberSecret :: String
  }
$(deriveJSON defaultOptions ''EnrollReqDTO)

data EnrollResDTO = EnrollResDTO {
    accessToken :: Text
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''EnrollResDTO)