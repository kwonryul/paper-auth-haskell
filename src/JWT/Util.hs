{-# LANGUAGE OverloadedStrings #-}

module JWT.Util(
    JWTUtilI(
        generateRefreshTokenCookie
      )
) where

import Monad.ProfileT

import Web.Cookie

import Data.Text
import Data.Text.Encoding
import Data.Proxy

class Profile p => JWTUtilI p where
    generateRefreshTokenCookie :: Proxy p -> Text -> SetCookie
    generateRefreshTokenCookie = generateRefreshTokenCookieImpl

generateRefreshTokenCookieImpl :: Proxy p -> Text -> SetCookie
generateRefreshTokenCookieImpl _ refreshToken =
    defaultSetCookie {
        setCookieName = "Paper-Refresh-Token"
      , setCookieValue = Data.Text.Encoding.encodeUtf8 refreshToken
      , setCookiePath = Just "/"
      , setCookieHttpOnly = True
      , setCookieSecure = True
      , setCookieSameSite = Just sameSiteStrict
      }