{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module User.DTO where

import Definition

import Data.Aeson
import Data.Aeson.TH

import Data.Text
import Data.Time

$(defineDTO "user/getUserInfoResDTO.dto")
$(deriveJSON defaultOptions ''GetUserInfoResDTO)

$(defineDTO "user/patchUserInfoReqDTO.dto")
$(deriveJSON defaultOptions ''PatchUserInfoReqDTO)

$(defineDTO "user/enrollReqDTO.dto")
$(deriveJSON defaultOptions ''EnrollReqDTO)

$(defineDTO "user/enrollResDTO.dto")
$(deriveJSON defaultOptions ''EnrollResDTO)