{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Verification.DTO where

import Definition

import Data.Aeson
import Data.Aeson.TH

$(defineDTO "verification/verifyRequestReqDTO.dto")
$(deriveJSON defaultOptions ''VerifyRequestReqDTO)

$(defineDTO "verification/verifyCheckReqDTO.dto")
$(deriveJSON defaultOptions ''VerifyCheckReqDTO)

$(defineDTO "verification/verifyCheckResDTO.dto")
$(deriveJSON defaultOptions ''VerifyCheckResDTO)
