{-# LANGUAGE OverloadedStrings #-}

module CORS(
    CORSI(
        corsMiddleware
      )
) where

import Monad.ProfileT

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.HTTP.Types.Method

import Data.Proxy

class Profile p => CORSI p where
    corsMiddleware :: Proxy p -> Middleware
    corsMiddleware = corsMiddlewareImpl

corsMiddlewareImpl :: Proxy p -> Middleware
corsMiddlewareImpl _ = cors (\_ -> Just $ simpleCorsResourcePolicy {
      corsOrigins = Just ([
          "http://43.200.64.248"
        , "https://43.200.64.248"
        , "http://43.200.64.248:8080"
        , "https://43.200.64.248:3000"
        ], False)
    , corsMethods = [methodGet, methodPost, methodPatch, methodDelete, methodHead]
    , corsMaxAge = Just 3600
    , corsVaryOrigin = True
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    })