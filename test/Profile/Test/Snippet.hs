{-# LANGUAGE OverloadedStrings #-}

module Profile.Test.Snippet(
    generateSnippetM
  ) where

import Import
import Profile.Test.Import
import GlobalMonad
import Configurator

import Network.Wai
import Network.HTTP.Types

import Data.ByteString.Lazy.Char8
import Data.ByteString.Char8
import Data.ByteString.Builder
import Data.CaseInsensitive
import Data.IORef
import Data.Proxy
import System.Directory
import System.FilePath.Posix

generateSnippetM :: Context -> Middleware
generateSnippetM context app req sendResponse = do
    projectDir <- runGlobalMonad @Test context $ lookupRequiredGlobal (config context) "projectDir"
    let snippetsDir = projectDir ++ "generated/snippets/"
        maybeFilePath = lookup "Snippet-Path" $ requestHeaders req
    case maybeFilePath of
        Just fp -> do
            let filePath = snippetsDir ++ Data.ByteString.Char8.unpack fp
                req' = setRequestBodyChunks (bodyChunks' filePath) req
            globalLog profile context $ createDirectoryIfMissing True $ takeDirectory filePath
            globalLog profile context $ Data.ByteString.Char8.writeFile (filePath ++ "-request.adoc") $
                requestHeaders' req <> "\n\nBody:\n"
            globalLog profile context $ app req' $ \res -> do
                responseBody <- globalLog profile context $ responseBody' res
                globalLog profile context $ createDirectoryIfMissing True $ takeDirectory filePath
                globalLog profile context $ Data.ByteString.Char8.writeFile (filePath ++ "-response.adoc") $
                    responseHeaders' res <> "\n\nBody:\n" <> responseBody
                globalLog profile context $ sendResponse res
        Nothing ->
            globalLog profile context $ app req $ \res -> sendResponse res
    where
        profile :: Proxy Test
        profile = Proxy
        bodyChunks' :: FilePath -> IO Data.ByteString.Char8.ByteString
        bodyChunks' filePath = do
            globalLog profile context $ createDirectoryIfMissing True $ takeDirectory filePath
            chunk <- globalLog profile context $ getRequestBodyChunk req
            globalLog profile context $ Data.ByteString.Char8.appendFile (filePath ++ "-request.adoc") chunk
            return chunk

            

showHeader :: Header -> Data.ByteString.Char8.ByteString
showHeader (name, value) = Data.CaseInsensitive.foldedCase name <> ": " <> value

requestHeaders' :: Request -> Data.ByteString.Char8.ByteString
requestHeaders' req =
    Data.ByteString.Char8.concat [
        "Method:\t", requestMethod req, "\n"
      , "Path:\t", rawPathInfo req, "\n"
      , "Headers:\n\t", Data.ByteString.Char8.intercalate "\n\t" $ showHeader <$> requestHeaders req, "\n"
      ]

responseHeaders' :: Response -> Data.ByteString.Char8.ByteString
responseHeaders' res =
    Data.ByteString.Char8.concat [
        "Status:\t", Data.ByteString.Char8.pack $ show $ responseStatus res, "\n"
      , "Headers:\n\t", Data.ByteString.Char8.intercalate "\n\t" $ showHeader <$> responseHeaders res, "\n"
      ]

responseBody' :: Response -> IO Data.ByteString.Char8.ByteString
responseBody' res =
    let (_, _, body) = responseToStream res in
    body $ \f -> do
        content <- newIORef mempty
        f (\chunk -> modifyIORef' content (<> chunk)) (return ())
        Data.ByteString.Lazy.Char8.toStrict . toLazyByteString <$> readIORef content