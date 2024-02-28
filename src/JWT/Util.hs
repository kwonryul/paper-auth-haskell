{-# LANGUAGE OverloadedStrings #-}

module JWT.Util(
    generateRefreshTokenCookie
) where

import Web.Cookie

import Data.Text
import Data.Text.Encoding

generateRefreshTokenCookie :: Text -> SetCookie
generateRefreshTokenCookie refreshToken =
    defaultSetCookie {
        setCookieName = "Paper-Refresh-Token"
      , setCookieValue = Data.Text.Encoding.encodeUtf8 refreshToken
      , setCookiePath = Just "/"
      , setCookieHttpOnly = True
      , setCookieSecure = True
      , setCookieSameSite = Just sameSiteStrict
      }