{-# LANGUAGE DataKinds #-}

module Test.JWT(
    jwtSpec
  ) where

import qualified JWT.Controller
import JWT.Controller hiding (API)

import Authentication
import Lib
import Import
import JWT.DTO

import Profile.Test.Snippet

import Servant
import Servant.Client
import Network.Wai.Handler.Warp
import Network.HTTP.Client

import Test.Hspec

import Control.Exception

type APIC = "jwt" :> Header "Snippet-Path" String :> JWTAPI'
type JWTAPI' = IssueJWT
    :<|> Header "Cookie" String :> RefreshJWT
    :<|> Header "Cookie" String :> Header "Authorization" String :> InvalidateJWT
    :<|> "indirect" :> (
        IndirectRequestJWT
        :<|> IndirectIssueJWT
    )
type APIS = "jwt" :> JWT.Controller.API

jwtApp :: forall profile. LibI profile => Servant.Proxy profile -> Import.Context -> Application
jwtApp profile ctx = generateSnippetM ctx $ serveWithContext (Servant.Proxy :: Servant.Proxy APIS)
    (authContext (Servant.Proxy :: Servant.Proxy profile) ctx) $ jwtServer profile ctx

jwtServer :: LibI profile => Servant.Proxy profile -> Import.Context -> Server APIS
jwtServer profile ctx = JWT.Controller.server profile ctx

withJWTApp :: LibI profile => Servant.Proxy profile -> (Network.Wai.Handler.Warp.Port -> IO ()) -> IO ()
withJWTApp profile action = do
    Resources { Lib.context = ctx } <- getAllResources profile
    Network.Wai.Handler.Warp.testWithApplication (pure $ jwtApp profile ctx) action

jwtSpec :: LibI profile => Servant.Proxy profile -> Spec
jwtSpec profile = around (withJWTApp profile) $ do
    let clients = client (Servant.Proxy :: Servant.Proxy APIC)
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })
    describe "jwt" $ do
        it "issueJWT" $ \port -> do
            result <- runClientM (issueJWTC clients) (clientEnv port)
            evaluate result `shouldReturn` result
        it "refreshJWT" $ \port -> do
            result <- runClientM (refreshJWTC clients) (clientEnv port)
            evaluate result `shouldReturn` result
        it "invalidateJWT" $ \port -> do
            result <- runClientM (invalidateJWTC clients) (clientEnv port)
            evaluate result `shouldReturn` result

issueJWTC :: Client ClientM APIC -> ClientM IssueJWTResDTO
issueJWTC clients =
    let f :<|> _ = clients $ Just "jwt/issueJWT" in
    getResponse <$> (f $ IssueJWTReqDTO "id12345" "pw12345")

refreshJWTC :: Client ClientM APIC -> ClientM RefreshJWTResDTO
refreshJWTC clients =
    let _ :<|> f :<|> _ = clients $ Just "jwt/refreshJWT" in
    getResponse <$> f (Just "Paper-Refresh-Token=this is dummy refreshToken: 7")

invalidateJWTC :: Client ClientM APIC -> ClientM NoContent
invalidateJWTC clients =
    let _ :<|> _ :<|> f :<|> _ = clients $ Just "jwt/invalidateJWT" in
    f (Just "Paper-Refresh-Token=this is dummy refreshToken: 7") (Just "Bearer this is dummy accessToken: 7")