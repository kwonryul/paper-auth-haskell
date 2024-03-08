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

import Definition

$(defineDTO "jwt/issueJWTReqDTO.dto")
$(deriveJSON defaultOptions ''IssueJWTReqDTO)

$(defineDTO "jwt/issueJWTResDTO.dto")
$(deriveJSON defaultOptions ''IssueJWTResDTO)

$(defineDTO "jwt/refreshJWTResDTO.dto")
$(deriveJSON defaultOptions ''RefreshJWTResDTO)