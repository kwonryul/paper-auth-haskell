{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module User.DTO(
    EnrollReqDTO(
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

$(defineDTO "user/enrollReqDTO.dto")
$(deriveJSON defaultOptions ''EnrollReqDTO)

$(defineDTO "user/enrollResDTO.dto")
$(deriveJSON defaultOptions ''EnrollResDTO)