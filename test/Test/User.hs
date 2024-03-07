{-# LANGUAGE DataKinds #-}

module Test.User(
    userSpec
  ) where

import qualified User.Controller

import Lib
import Import
import User.DTO

import Profile.Test.Snippet

import Servant
import Servant.Client
import Network.Wai.Handler.Warp
import Network.HTTP.Client

import Test.Hspec

import Control.Exception

type APIC = "user" :> Header "Snippet-Path" String :> User.Controller.API
type APIS = "user" :> User.Controller.API

userApp :: LibI profile => Servant.Proxy profile -> Import.Context -> Application
userApp profile ctx = generateSnippetM ctx $ serve (Servant.Proxy :: Servant.Proxy APIS) $ userServer profile ctx

userServer :: LibI profile => Servant.Proxy profile -> Import.Context -> Server APIS
userServer profile ctx = User.Controller.server profile ctx

withUserApp :: LibI profile => Servant.Proxy profile -> (Network.Wai.Handler.Warp.Port -> IO ()) -> IO ()
withUserApp profile action = do
    Resources { Lib.context = ctx } <- getAllResources profile
    Network.Wai.Handler.Warp.testWithApplication (pure $ userApp profile ctx) action

userSpec :: LibI profile => Servant.Proxy profile -> Spec
userSpec profile = around (withUserApp profile) $ do
    let clients = client (Servant.Proxy :: Servant.Proxy APIC)
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })
    describe "user/verify" $ do
        it "verifyRequest" $ \port -> do
            result <- runClientM (verifyRequestC clients) (clientEnv port)
            evaluate result `shouldReturn` result
        it "verifyCheck" $ \port -> do
            result <- runClientM (verifyCheckC clients) (clientEnv port)
            evaluate result `shouldReturn` result
    describe "user" $ do
        it "enroll" $ \port -> do
            result <- runClientM (enrollC clients) (clientEnv port)
            evaluate result `shouldReturn` result

verifyRequestC :: Client ClientM APIC -> ClientM NoContent
verifyRequestC clients =
    let (f :<|> _) :<|> _ = clients $ Just "user/verifyRequest" in
    f $ VerifyRequestReqDTO "010-5432-7890"

verifyCheckC :: Client ClientM APIC -> ClientM VerifyCheckResDTO
verifyCheckC clients =
    let (_ :<|> f) :<|> _ = clients $ Just "user/verifyCheck" in
    f $ VerifyCheckReqDTO "010-5432-7890" "123456"

enrollC :: Client ClientM APIC -> ClientM EnrollResDTO
enrollC clients =
    let (_ :<|> _) :<|> f = clients $ Just "user/enroll" in
    getResponse <$> (f $ EnrollReqDTO "test0id" "test0pw" "test0name" "010-5432-7890" "123456")