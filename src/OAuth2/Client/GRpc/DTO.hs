{-# LANGUAGE TemplateHaskell #-}

module OAuth2.Client.GRpc.DTO where

import Definition

import Data.Aeson
import Data.Aeson.TH

import Data.Text


$(defineDTO "oauth2/client/grpc/issueJWTResDTO.dto")
$(deriveJSON defaultOptions ''IssueJWTResDTO)