{-# LANGUAGE OverloadedStrings #-}

module Middleware.CORS(
    CORSI(
        corsM
      )
) where

import CallStack
import GlobalMonad
import Import

import Network.Wai
import Network.URI
import Network.HTTP.Types.Method
import Network.HTTP.Types

import Data.ByteString.Char8
import Data.Char
import Data.CaseInsensitive
import Data.List
import Data.Proxy
import GHC.Stack

class GlobalMonadI p => CORSI p where
    corsM :: HasCallStack => Proxy p -> Context -> Middleware
    corsM = corsMImpl


corsMImpl :: forall p. (HasCallStack, CORSI p) => Proxy p -> Context -> Middleware
corsMImpl p ctx app req sendResponse = do
    let allowOrigins = [
            "http://43.200.64.248:80"
          , "https://43.200.64.248:443"
          , "http://43.200.64.248:8080"
          , "https://43.200.64.248.3000"
          ]
        allowMethods = [
            "GET"
          , "POST"
          , "PATCH"
          , "DELETE"
          , "HEAD"
          , "OPTIONS"
          ]
        allowHeaders = [
            "Content-Type"
          , "Authorization"
          ]
        corsHeaders = [
            ("Access-Control-Allow-Methods", Data.ByteString.Char8.intercalate ", " allowMethods)
          , ("Access-Control-Allow-Headers", Data.ByteString.Char8.intercalate ", " allowHeaders)
          , ("Access-Control-Allow-Credentials", "true")
          , ("Access-Control-Max-Age", "3600")
          , ("Vary", "Origin")
          ]
        simpleHeaders = [
            "Accept"
          , "Accept-Language"
          , "Content-Language"
          ]
    if requestMethod req == methodOptions then do
        let requestOriginTuple' = (lookup "Origin" $ requestHeaders req) >>= (addPortToUrl . Data.ByteString.Char8.unpack)
            requestMethod' = lookup "Access-Control-Request-Method" $ requestHeaders req
            requestingHeaders' = splitAndTrimByteString <$> (lookup "Access-Control-Request-Headers" $ requestHeaders req)
        case (requestOriginTuple', requestMethod', requestingHeaders') of
            (Just (modifiedFlag, requestOriginModified), Just method, Just requestingHeaders)  -> do
                let originMatched' = Data.List.find (`Data.ByteString.Char8.isPrefixOf` requestOriginModified) allowOrigins
                    headersAllowed = Prelude.filter (\h -> mk h `Prelude.elem` (mk <$> allowHeaders ++ simpleHeaders)) requestingHeaders
                case originMatched' of
                    Just originMatched ->
                        if Prelude.any ((mk method) ==) (mk <$> allowMethods) &&
                            Prelude.length headersAllowed == Prelude.length requestingHeaders
                        then do
                            recoveredOriginMatched <- runGlobalMonad @p ctx $ maybeToGlobalMonad (recover modifiedFlag originMatched) $ GlobalError "cors allowed origin not valid" (callStack' p)
                            globalLog p ctx $ sendResponse $ responseLBS status200
                                (("Access-Control-Allow-Origin", recoveredOriginMatched) : ("Access-Control-Allow-Credentials", "true") : corsHeaders) ""
                        else
                            globalLog p ctx $ sendResponse $ responseLBS status400 [] "method / headers not allowed"
                    Nothing ->
                        globalLog p ctx $ sendResponse $ responseLBS status400 [] "origin not allowed"
            _ -> do
                globalLog p ctx $ sendResponse $ responseLBS status400 [] "OPTIONS should be sent with Origin"
    else
        let requestOriginTuple' = (lookup "Origin" $ requestHeaders req) >>= (addPortToUrl . Data.ByteString.Char8.unpack) in
        case requestOriginTuple' of
            Just (modifiedFlag, requestOriginModified) -> do
                let originMatched' = Data.List.find (`Data.ByteString.Char8.isPrefixOf` requestOriginModified) allowOrigins
                case originMatched' of
                    Just originMatched ->
                        if  Prelude.any ((Data.ByteString.Char8.map toLower $ requestMethod req) ==) (Data.ByteString.Char8.map toLower <$> allowMethods) then do
                            recoveredOriginMatched <- runGlobalMonad @p ctx $ maybeToGlobalMonad (recover modifiedFlag originMatched) $ GlobalError "cors allowed origin not valid" (callStack' p)
                            globalLog p ctx $ app req $ \res -> sendResponse $ mapResponseHeaders (\h ->
                                (("Access-Control-Allow-Origin", recoveredOriginMatched) : ("Access-Control-Allow-Credentials", "true"): h)
                                ) res
                        else
                            globalLog p ctx $ sendResponse $ responseLBS status400 [] "method not allowed"
                    Nothing ->
                            globalLog p ctx $ sendResponse $ responseLBS status400 [] "origin not allowed"
            Nothing ->
                globalLog p ctx $ app req $ \res -> sendResponse res
    where
        addPortToUrl :: String -> Maybe (Bool, Data.ByteString.Char8.ByteString)
        addPortToUrl urlStr = do
            uri <- parseURI urlStr
            auth <- uriAuthority uri
            let scheme' = uriScheme uri
                host = uriRegName auth
            (modifiedFlag, port) <-
                if uriPort auth == "" then
                    if scheme' == "http:" then return (True, ":80")
                    else if scheme' == "https:" then return (True, ":443")
                    else Nothing
                else
                    return $ (False, uriPort auth)
            return $ (modifiedFlag
              , Data.ByteString.Char8.pack $ scheme' ++ "//" ++ host ++ port ++ uriPath uri)
        recover :: Bool -> ByteString -> Maybe ByteString
        recover False urlStr = return urlStr
        recover True urlStr = do
            uri <- parseURI $ Data.ByteString.Char8.unpack urlStr
            auth <- uriAuthority uri
            let scheme' = uriScheme uri
                host = uriRegName auth
            return $ Data.ByteString.Char8.pack $ scheme' ++ "//" ++ host ++ uriPath uri
        splitAndTrimByteString :: Data.ByteString.Char8.ByteString -> [Data.ByteString.Char8.ByteString]
        splitAndTrimByteString bs = Data.List.filter ("" /=) $ trim <$> Data.ByteString.Char8.splitWith (\c -> c == ',') bs
        trim :: Data.ByteString.Char8.ByteString -> Data.ByteString.Char8.ByteString
        trim = f . f
        f :: Data.ByteString.Char8.ByteString -> Data.ByteString.Char8.ByteString
        f = Data.ByteString.Char8.dropWhile isSpace . Data.ByteString.Char8.reverse