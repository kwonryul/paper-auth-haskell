{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module User.DTO where

import Definition

import Data.Aeson
import Data.Aeson.TH

import Data.Text

$(defineDTO "user/enrollReqDTO.dto")
$(deriveJSON defaultOptions ''EnrollReqDTO)

$(defineDTO "user/enrollResDTO.dto")
$(deriveJSON defaultOptions ''EnrollResDTO)