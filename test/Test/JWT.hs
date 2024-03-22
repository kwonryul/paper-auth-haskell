{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.JWT(
    jwtSpec
  ) where

import qualified JWT.Controller
import JWT.Controller hiding (API)

import JWT.DTO
import Profile.Test.Snippet
import Authentication
import Import
import Lib
import MIME


import Servant
import Servant.Client
import Network.Wai.Handler.Warp
import Network.HTTP.Client
import Web.Cookie

import Test.Hspec

import Data.ByteString.Char8
import Control.Exception

type IssueJWTC = "jwt" :> "issue" :> Header "Snippet-Path" String :>
    ReqBody '[PrettyJSON] IssueJWTReqDTO :> Post '[PrettyJSON] (Headers '[Header "Set-Cookie" SetCookie] IssueJWTResDTO)
type RefreshJWTC = "jwt" :> "refresh" :> Header "Snippet-Path" String :>
    Header "Cookie" String :> Post '[PrettyJSON] (Headers '[Header "Set-Cookie" SetCookie] RefreshJWTResDTO)
type InvalidateJWTC = "jwt" :> "invalidate" :> Header "Snippet-Path" String :>
    Header "Cookie" String :> Header "Authorization" String :> Delete '[PlainText] NoContent

issueJWTC :: Client ClientM IssueJWTC
issueJWTC = client (Servant.Proxy :: Servant.Proxy IssueJWTC)

refreshJWTC :: Client ClientM RefreshJWTC
refreshJWTC = client (Servant.Proxy :: Servant.Proxy RefreshJWTC)

invalidateJWTC :: Client ClientM InvalidateJWTC
invalidateJWTC = client (Servant.Proxy :: Servant.Proxy InvalidateJWTC)

type APIS = "jwt" :> JWT.Controller.API

jwtApp :: forall profile. LibI profile => Servant.Proxy profile -> Import.Context -> Application
jwtApp profile ctx = generateExampleSnippetM (config ctx) $ serveWithContext (Servant.Proxy :: Servant.Proxy APIS)
    (authContext (Servant.Proxy :: Servant.Proxy profile) ctx) $ jwtServer profile ctx

jwtServer :: LibI profile => Servant.Proxy profile -> Import.Context -> Server APIS
jwtServer profile ctx = JWT.Controller.server profile ctx

withJWTApp :: LibI profile => Servant.Proxy profile -> (Network.Wai.Handler.Warp.Port -> IO ()) -> IO ()
withJWTApp profile action = do
    Resources { Lib.context = ctx } <- getAllResources profile
    Network.Wai.Handler.Warp.testWithApplication (pure $ jwtApp profile ctx) action

jwtSpec :: LibI profile => Servant.Proxy profile -> Spec
jwtSpec profile = around (withJWTApp profile) $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })
    describe "jwt" $ do
        it "issueJWT" $ \port -> issueJWTTest $ clientEnv port
        it "refreshJWT" $ \port -> refreshJWTTest $ clientEnv port
        it "invalidateJWT" $ \port -> invalidateJWTTest $ clientEnv port

issueJWTTest :: ClientEnv -> IO ()
issueJWTTest clientEnv = do
    result <- runClientM (issueJWTC (Just "jwt/issueJWT") $
        IssueJWTReqDTO "testId" "testPw") clientEnv
    case result of
        Left err -> throwIO err
        Right dto -> do
            getResponse dto `shouldBe` IssueJWTResDTO "this is dummy accessToken: 1"
            Prelude.any (\(name, value) ->
                    name == "Set-Cookie" &&
                    Data.ByteString.Char8.isInfixOf "Paper-Refresh-Token" value
                ) (getHeaders dto) `shouldBe` True

refreshJWTTest :: ClientEnv -> IO ()
refreshJWTTest clientEnv = do
    result <- runClientM (refreshJWTC (Just "jwt/refreshJWT")
        (Just "Paper-Refresh-Token=this is dummy refreshToken: 1")
        ) clientEnv
    case result of
        Left err -> throwIO err
        Right dto -> do
            getResponse dto `shouldBe` RefreshJWTResDTO "this is dummy accessToken: 1"
            Prelude.any (\(name, value) ->
                    name == "Set-Cookie" &&
                    Data.ByteString.Char8.isInfixOf "Paper-Refresh-Token" value
                ) (getHeaders dto) `shouldBe` True

invalidateJWTTest :: ClientEnv -> IO ()
invalidateJWTTest clientEnv= do
    result <- runClientM (invalidateJWTC (Just "jwt/invalidateJWT")
        (Just "Paper-Refresh-Token=this is dummy refreshToken: 1")
        (Just "Bearer this is dummy accessToken: 1")
        ) clientEnv
    case result of
        Left err -> throwIO err
        Right dto -> dto `shouldBe` NoContent