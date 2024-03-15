{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module JWT.DTO where

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