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
      )
  , RefreshJWTResDTO(
        RefreshJWTResDTO
      , accessToken
      )
) where

import Data.Aeson
import Data.Aeson.TH

import Data.Text

data IssueJWTReqDTO = IssueJWTReqDTO {
    paperId :: String
  , password :: String
  }
$(deriveJSON defaultOptions ''IssueJWTReqDTO)

data IssueJWTResDTO = IssueJWTResDTO {
    accessToken :: Text
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''IssueJWTResDTO)

data RefreshJWTResDTO = RefreshJWTResDTO {
    accessToken :: Text
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''RefreshJWTResDTO)