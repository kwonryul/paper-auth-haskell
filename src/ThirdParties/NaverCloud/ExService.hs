{-# LANGUAGE OverloadedStrings #-}

module ThirdParties.NaverCloud.ExService(
    NaverCloudExServiceI(
        getNaverCloudSignature
      , applyNaverCloudHeaders
      )
) where

import Util
import Configurator

import Network.HTTP.Types
import Crypto.Hash
import Crypto.MAC.HMAC
import Data.ByteArray.Encoding

import Data.Time
import Data.Proxy
import Data.ByteString.Char8

class (UtilI p, ConfiguratorI p) => NaverCloudExServiceI p where
    getNaverCloudSignature :: Proxy p -> String -> String -> StdMethod -> String -> Int -> String
    getNaverCloudSignature = getNaverCloudSignatureImpl
    applyNaverCloudHeaders :: Proxy p -> String -> String -> StdMethod -> String -> UTCTime -> (Maybe String -> Maybe String -> Maybe String -> a) -> a
    applyNaverCloudHeaders = applyNaverCloudHeadersImpl

getNaverCloudSignatureImpl :: NaverCloudExServiceI p => Proxy p -> String -> String -> StdMethod -> String -> Int -> String
getNaverCloudSignatureImpl _ accessKey secretKey method urlPath currentTimeMillis =
    let raw = show method ++ " " ++ urlPath ++ "\n" ++ show currentTimeMillis ++ "\n" ++ accessKey
        hmacDigest = hmacGetDigest $ (hmac (Data.ByteString.Char8.pack secretKey) $ Data.ByteString.Char8.pack raw :: HMAC SHA256) in
    Data.ByteString.Char8.unpack $ (convertToBase Base64 hmacDigest :: Data.ByteString.Char8.ByteString)

{-
First argument: X-NCP-APIGW-TIMESTAMP
Second argument: X-NCP-IAM-ACCESS-KEY
Third argument: X-NCP-APIGW-SIGNATURE-V2
-}
applyNaverCloudHeadersImpl :: NaverCloudExServiceI p => Proxy p -> String -> String -> StdMethod -> String -> UTCTime -> (Maybe String -> Maybe String -> Maybe String -> a) -> a
applyNaverCloudHeadersImpl profile accessKey secretKey method urlPath currentUTC f = do
    let currentTimeMillis = toCurrentTimeMillis profile currentUTC
        signature = getNaverCloudSignature profile accessKey secretKey method urlPath currentTimeMillis
    f (Just $ show currentTimeMillis) (Just accessKey) (Just signature)