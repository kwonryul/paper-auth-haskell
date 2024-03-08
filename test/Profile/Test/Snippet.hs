{-# LANGUAGE OverloadedStrings #-}

module Profile.Test.Snippet(
    generateExampleSnippetM
  ) where

import Import
import Profile.Test.Import
import GlobalMonad

import Network.Wai
import Network.HTTP.Types

import Data.ByteString.Lazy.Char8
import Data.ByteString.Char8
import Data.ByteString.Builder
import Data.Text.Encoding
import Data.CaseInsensitive
import Data.IORef
import Data.Proxy
import System.Directory
import System.FilePath.Posix
import System.Environment

generateExampleSnippetM :: Context -> Middleware
generateExampleSnippetM ctx app req sendResponse = do
    homeDir <- globalLog profile ctx $ getEnv "HOME"
    projectDir <- globalLog profile ctx $ Prelude.readFile $ homeDir ++ "/.paper-auth/project-directory"
    let exampleSnippetsDir = projectDir ++ "generated/snippets/examples/"
        maybeFilePath = lookup "Snippet-Path" $ requestHeaders req
    case maybeFilePath of
        Just fp -> do
            let filePath = exampleSnippetsDir ++ Data.ByteString.Char8.unpack fp
                req' = setRequestBodyChunks (bodyChunks' filePath) req
            globalLog profile ctx $ createDirectoryIfMissing True $ takeDirectory filePath
            globalLog profile ctx $ Data.ByteString.Char8.writeFile (filePath ++ "-request.adoc") $ requestHeaders' req
            globalLog profile ctx $ app req' $ \res -> do
                responseBody' <- globalLog profile ctx $ getResponseBody res
                let responseBody = if responseBody' == "" then "" else "\n\n" <> responseBody'
                globalLog profile ctx $ createDirectoryIfMissing True $ takeDirectory filePath
                globalLog profile ctx $ Data.ByteString.Char8.writeFile (filePath ++ "-response.adoc") $ responseHeaders' res <> responseBody
                globalLog profile ctx $ sendResponse res
        Nothing ->
            globalLog profile ctx $ app req $ \res -> sendResponse res
    where
        profile :: Proxy Test
        profile = Proxy
        bodyChunks' :: FilePath -> IO Data.ByteString.Char8.ByteString
        bodyChunks' filePath = do
            globalLog profile ctx $ createDirectoryIfMissing True $ takeDirectory filePath
            chunk <- globalLog profile ctx $ getRequestBodyChunk req
            globalLog profile ctx $ Data.ByteString.Char8.appendFile (filePath ++ "-request.adoc") chunk
            return chunk

            

showHeader :: Header -> Data.ByteString.Char8.ByteString
showHeader (name, value) = Data.CaseInsensitive.foldedCase name <> ": " <> value

requestHeaders' :: Request -> Data.ByteString.Char8.ByteString
requestHeaders' req =
    let queryString' = queryString req
        (queryString'', queryStringTrailingNewLines) =
            case queryString' of
                [] -> ("", "")
                _ -> (Data.ByteString.Char8.intercalate "\n" $ showQueryItem <$> queryString', "\n\n")
        headerList = Prelude.filter (\(n, _) -> n /= "Snippet-Path") $ requestHeaders req
        contentLength = read @Int . Data.ByteString.Char8.unpack <$> (lookup "Content-Length" $ requestHeaders req)
        trailingNewLines =
            case contentLength of
                Nothing -> "\n\n"
                Just x -> if x == 0 then "" else "\n\n"
    in
    Data.ByteString.Char8.concat [
        requestMethod req, "\t"
      , Data.ByteString.Char8.intercalate "/" $ Data.Text.Encoding.encodeUtf8 <$> pathInfo req, "\n\n"
      , queryString'', queryStringTrailingNewLines
      , Data.ByteString.Char8.intercalate "\n" $ showHeader <$> headerList
      , trailingNewLines
      ]
    where
        showQueryItem :: QueryItem -> Data.ByteString.Char8.ByteString
        showQueryItem (key, Just value) = Data.ByteString.Char8.concat [key, " : ", value]
        showQueryItem (key , Nothing) = key

responseHeaders' :: Response -> Data.ByteString.Char8.ByteString
responseHeaders' res =
    let status = responseStatus res in
    Data.ByteString.Char8.concat [
        Data.ByteString.Char8.pack $ show $ statusCode status, "\t"
      , statusMessage status, "\n\n"
      , Data.ByteString.Char8.intercalate "\n" $ showHeader <$> responseHeaders res
      ]

getResponseBody :: Response -> IO Data.ByteString.Char8.ByteString
getResponseBody res =
    let (_, _, body) = responseToStream res in
    body $ \f -> do
        content <- newIORef mempty
        f (\chunk -> modifyIORef' content (<> chunk)) (return ())
        Data.ByteString.Lazy.Char8.toStrict . toLazyByteString <$> readIORef content