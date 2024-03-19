{-# LANGUAGE OverloadedStrings #-}

module Middleware.Utf8(
    Utf8I(
        utf8M
      )
) where

import Network.Wai
import Network.HTTP.Types

import Data.ByteString.Char8
import Data.Proxy
import GHC.Stack

class Utf8I p where
    utf8M :: HasCallStack => Proxy p -> Middleware
    utf8M = utf8MImpl

utf8MImpl :: (HasCallStack, Utf8I p) => Proxy p -> Middleware
utf8MImpl _ app req sendResponse = app req $ (\res -> do
    let res' = case lookup hContentType $ responseHeaders res of
            Just ct | isTextBasedContentType ct ->
                mapResponseHeaders appendUtf8 res
            _ -> res
    sendResponse res'
    )
    where
        isTextBasedContentType :: Data.ByteString.Char8.ByteString -> Bool
        isTextBasedContentType ct =
            Data.ByteString.Char8.isPrefixOf "text/" ct ||
                Data.ByteString.Char8.isPrefixOf "application/json" ct
        appendUtf8 :: ResponseHeaders -> ResponseHeaders
        appendUtf8 [] = []
        appendUtf8 ((name, value) : t) =
            if name == "Content-Type" then
                (name, value <> "; charset=utf-8") : t
            else
                (name, value) : t