{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.User(
    userSpec
  ) where

import qualified User.Controller

import Profile.Test.Snippet
import User.DTO
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

import Control.Exception
import Data.ByteString.Char8
import Data.Text

type EnrollC = "user" :> "enroll" :> Header "Snippet-Path" String :>
    ReqBody '[PrettyJSON] EnrollReqDTO :> Post '[PrettyJSON] (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)

enrollC :: Client ClientM EnrollC
enrollC = client (Servant.Proxy :: Servant.Proxy EnrollC)

type APIS = "user" :> User.Controller.API

userApp :: forall profile. LibI profile => Servant.Proxy profile -> Import.Context -> Application
userApp profile ctx = generateExampleSnippetM ctx $ serveWithContext (Servant.Proxy :: Servant.Proxy APIS)
    (authContext(Servant.Proxy :: Servant.Proxy profile) ctx) $ userServer profile ctx

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
    describe "user" $ do
        it "enroll" $ \port -> enrollTest $ clientEnv port

enrollTest :: ClientEnv -> IO ()
enrollTest clientEnv = do
    result <- runClientM (enrollC (Just "user/enroll")
        (EnrollReqDTO "test_id" "test_pw" "010-1234-0987" "123456")
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