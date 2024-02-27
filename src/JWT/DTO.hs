{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module JWT.DTO(
    IssueJWTReqDTO(
        IssueJWTReqDTO
      , paperId
      , password
      )
  , IssueJWTResDTO(
        IssueJWTResDTO
      , accessToken
      , refreshToken
      , csrfToken
      )
  , FromJWTDTO(
        fromJWTDTO
      )
) where

import JWT.Model

import Data.Aeson
import Data.Aeson.TH

import Data.Text

data IssueJWTReqDTO = IssueJWTReqDTO {
    paperId :: String
  , password :: String
  }
$(deriveFromJSON defaultOptions ''IssueJWTReqDTO)

data IssueJWTResDTO = IssueJWTResDTO {
    accessToken :: Text
  , refreshToken :: Text
  , csrfToken :: Text
  }
$(deriveToJSON defaultOptions ''IssueJWTResDTO)

instance FromJWTDTO IssueJWTResDTO where
    fromJWTDTO (JWTDTO { accessToken, refreshToken, csrfToken }) =
        IssueJWTResDTO { accessToken, refreshToken, csrfToken }