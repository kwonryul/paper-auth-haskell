{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Verification.DTO(
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
) where

import Definition

import Data.Aeson
import Data.Aeson.TH

$(defineDTO "verification/verifyRequestReqDTO.dto")
$(deriveJSON defaultOptions ''VerifyRequestReqDTO)

$(defineDTO "verification/verifyCheckReqDTO.dto")
$(deriveJSON defaultOptions ''VerifyCheckReqDTO)

$(defineDTO "verification/verifyCheckResDTO.dto")
$(deriveJSON defaultOptions ''VerifyCheckResDTO)
