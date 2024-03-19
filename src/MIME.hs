{-# LANGUAGE OverloadedStrings #-}

module MIME(
    PrettyJSON
  , HTMLBlaze
) where

import Prelude ((.))

import Servant
import Network.HTTP.Media
import Text.Blaze
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8

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

data HTMLBlaze

instance Servant.Accept HTMLBlaze where
    contentType _ = "text" // "html"

instance {-# OVERLAPPABLE #-} ToMarkup a => MimeRender HTMLBlaze a where
    mimeRender _ = renderHtml . Text.Blaze.Html.toHtml

instance {-# OVERLAPPING #-} MimeRender HTMLBlaze Text.Blaze.Html.Html where
    mimeRender _ = renderHtml