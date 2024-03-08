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

import Definition

import Data.Aeson
import Data.Aeson.TH

import Data.Text

$(defineDTO "user/verifyRequestReqDTO.dto")
$(deriveJSON defaultOptions ''VerifyRequestReqDTO)

$(defineDTO "user/verifyCheckReqDTO.dto")
$(deriveJSON defaultOptions ''VerifyCheckReqDTO)

$(defineDTO "user/verifyCheckResDTO.dto")
$(deriveJSON defaultOptions ''VerifyCheckResDTO)

$(defineDTO "user/enrollReqDTO.dto")
$(deriveJSON defaultOptions ''EnrollReqDTO)

$(defineDTO "user/enrollResDTO.dto")
$(deriveJSON defaultOptions ''EnrollResDTO)