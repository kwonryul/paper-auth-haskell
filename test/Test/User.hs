{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.User(
    userSpec
  ) where

import qualified User.Controller

import Lib
import Import
import User.DTO
import MIME

import Profile.Test.Snippet

import Servant
import Servant.Client
import Network.Wai.Handler.Warp
import Network.HTTP.Client
import Web.Cookie

import Test.Hspec

import Control.Exception
import Data.ByteString.Char8
import Data.Text

type VerifyRequestC = "user" :> "verify" :> "request" :> Header "Snippet-Path" String :>
    ReqBody '[PrettyJSON] VerifyRequestReqDTO :> Post '[PlainText] NoContent
type VerifyCheckC = "user" :> "verify" :> "check" :> Header "Snippet-Path" String :>
    ReqBody '[PrettyJSON] VerifyCheckReqDTO :> Post '[PrettyJSON] VerifyCheckResDTO
type EnrollC = "user" :> "enroll" :> Header "Snippet-Path" String :>
    ReqBody '[PrettyJSON] EnrollReqDTO :> Post '[PrettyJSON] (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)

verifyRequestC :: Client ClientM VerifyRequestC
verifyRequestC = client (Servant.Proxy :: Servant.Proxy VerifyRequestC)

verifyCheckC :: Client ClientM VerifyCheckC
verifyCheckC = client (Servant.Proxy :: Servant.Proxy VerifyCheckC)

enrollC :: Client ClientM EnrollC
enrollC = client (Servant.Proxy :: Servant.Proxy EnrollC)

type APIS = "user" :> User.Controller.API

userApp :: LibI profile => Servant.Proxy profile -> Import.Context -> Application
userApp profile ctx = generateExampleSnippetM ctx $ serve (Servant.Proxy :: Servant.Proxy APIS) $ userServer profile ctx

userServer :: LibI profile => Servant.Proxy profile -> Import.Context -> Server APIS
userServer profile ctx = User.Controller.server profile ctx

withUserApp :: LibI profile => Servant.Proxy profile -> (Network.Wai.Handler.Warp.Port -> IO ()) -> IO ()
withUserApp profile action = do
    Resources { Lib.context = ctx } <- getAllResources profile
    Network.Wai.Handler.Warp.testWithApplication (pure $ userApp profile ctx) action

userSpec :: LibI profile => Servant.Proxy profile -> Spec
userSpec profile = around (withUserApp profile) $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })
    describe "user/verify" $ do
        it "verifyRequest" $ \port -> verifyRequestTest $ clientEnv port
        it "verifyCheck" $ \port -> verifyCheckTest $ clientEnv port
    describe "user" $ do
        it "enroll" $ \port -> enrollTest $ clientEnv port

verifyRequestTest :: ClientEnv -> IO ()
verifyRequestTest clientEnv = do
    result <- runClientM (verifyRequestC (Just "user/verifyRequest")
        (VerifyRequestReqDTO "010-5432-7890")
        ) clientEnv
    case result of
        Left err -> throwIO err
        Right dto -> dto `shouldBe` NoContent

verifyCheckTest :: ClientEnv -> IO ()
verifyCheckTest clientEnv = do
    result <- runClientM (verifyCheckC (Just "user/verifyCheck")
        (VerifyCheckReqDTO "010-5432-7890" "123456")
        ) clientEnv
    case result of
        Left err -> throwIO err
        Right dto -> dto `shouldBe` VerifyCheckResDTO False 0

enrollTest :: ClientEnv -> IO ()
enrollTest clientEnv = do
    result <- runClientM (enrollC (Just "user/enroll")
        (EnrollReqDTO "test0id" "test0pw" "test0name" "010-5432-7890" "123456")
        ) clientEnv
    case result of
        Left err -> throwIO err
        Right dto -> do
            let EnrollResDTO { accessToken } = getResponse dto
            Data.Text.isPrefixOf "this is dummy accessToken: " accessToken `shouldBe` True
            Prelude.any (\(name, value) ->
                    name == "Set-Cookie" &&
                    Data.ByteString.Char8.isInfixOf "Paper-Refresh-Token" value
                ) (getHeaders dto) `shouldBe` True