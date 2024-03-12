{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Verification(
    verificationSpec
  ) where

import qualified Verification.Controller

import Profile.Test.Snippet
import Verification.DTO
import Import
import Lib
import MIME

import Servant
import Servant.Client
import Network.Wai.Handler.Warp
import Network.HTTP.Client

import Test.Hspec

import Control.Exception

type VerifyRequestC = "verification" :> "request" :> Header "Snippet-Path" String :>
    ReqBody '[PrettyJSON] VerifyRequestReqDTO :> Post '[PlainText] NoContent
type VerifyCheckC = "verification" :> "check" :> Header "Snippet-Path" String :>
    ReqBody '[PrettyJSON] VerifyCheckReqDTO :> Post '[PrettyJSON] VerifyCheckResDTO

verifyRequestC :: Client ClientM VerifyRequestC
verifyRequestC = client (Servant.Proxy :: Servant.Proxy VerifyRequestC)

verifyCheckC :: Client ClientM VerifyCheckC
verifyCheckC = client (Servant.Proxy :: Servant.Proxy VerifyCheckC)

type APIS = "verification" :> Verification.Controller.API

verificationApp :: LibI profile => Servant.Proxy profile -> Import.Context -> Application
verificationApp profile ctx = generateExampleSnippetM ctx $ serve (Servant.Proxy :: Servant.Proxy APIS) $ verificationServer profile ctx

verificationServer :: LibI profile => Servant.Proxy profile -> Import.Context -> Server APIS
verificationServer profile ctx = Verification.Controller.server profile ctx

withVerificationApp :: LibI profile => Servant.Proxy profile -> (Network.Wai.Handler.Warp.Port -> IO ()) -> IO ()
withVerificationApp profile action = do
    Resources { Lib.context = ctx } <- getAllResources profile
    Network.Wai.Handler.Warp.testWithApplication (pure $ verificationApp profile ctx) action

verificationSpec :: LibI profile => Servant.Proxy profile -> Spec
verificationSpec profile = around (withVerificationApp profile) $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })
    describe "verification" $ do
        it "verifyRequest" $ \port -> verifyRequestTest $ clientEnv port
        it "verifyCheck" $ \port -> verifyCheckTest $ clientEnv port

verifyRequestTest :: ClientEnv -> IO ()
verifyRequestTest clientEnv = do
    result <- runClientM (verifyRequestC (Just "verification/verifyRequest")
        (VerifyRequestReqDTO "010-1234-1234")
        ) clientEnv
    case result of
        Left err -> throwIO err
        Right dto -> dto `shouldBe` NoContent

verifyCheckTest :: ClientEnv -> IO ()
verifyCheckTest clientEnv = do
    result <- runClientM (verifyCheckC (Just "verification/verifyCheck")
        (VerifyCheckReqDTO "010-1234-1234" "123456")
        ) clientEnv
    case result of
        Left err -> throwIO err
        Right dto -> dto `shouldBe` VerifyCheckResDTO False 0