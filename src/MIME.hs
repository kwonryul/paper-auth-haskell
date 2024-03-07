{-# LANGUAGE OverloadedStrings #-}

module MIME(
    PrettyJSON
) where

import Prelude ()

import Servant
import Network.HTTP.Media

import Data.Aeson
import Data.Aeson.Encode.Pretty

import Data.Either

data PrettyJSON

instance Servant.Accept PrettyJSON where
    contentType _ = "application" // "json"

instance ToJSON a => MimeRender PrettyJSON a where
    mimeRender _ = encodePretty

instance FromJSON a => MimeUnrender PrettyJSON a where
    mimeUnrender _ input =
        case eitherDecode input of
            Left err -> Left err
            Right val -> case fromJSON val of
                Error err -> Left err
                Success x -> Right x