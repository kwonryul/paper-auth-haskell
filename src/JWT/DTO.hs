{-# LANGUAGE TemplateHaskell #-}

module JWT.DTO(
    IssueJWTReqDTO(
      IssueJWTReqDTO
    , paperId
    , password
    )
  , IssueJWTResDTO(
      IssueJWTResDTO
    , csrfToken
    )
) where

import Data.Aeson
import Data.Aeson.TH

import Data.Text

data IssueJWTReqDTO = IssueJWTReqDTO {
    paperId :: String
  , password :: String
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''IssueJWTReqDTO)

data IssueJWTResDTO = IssueJWTResDTO {
    accessToken :: Text
  , refreshToken :: Text
  , csrfToken :: Text
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''IssueJWTResDTO)