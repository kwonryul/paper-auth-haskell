{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PaperApp(
    PaperAppI(
        app
      )
  ) where

import qualified JWT.Controller
import JWT.Controller(JWTControllerI)
import qualified OAuth2.Client.Controller
import OAuth2.Client.Controller(OAuth2ClientControllerI)
import qualified User.Controller
import User.Controller(UserControllerI)
import qualified Verification.Controller
import Verification.Controller(VerificationControllerI)

import Middleware.CORS
import Middleware.Utf8
import Authentication
import Context
import PaperMonad

import Servant
import Servant.Static.TH.Internal.Mime
import Network.Wai.Application.Static
import WaiAppStatic.Types

import System.IO
import Data.ByteString
import Control.Monad.Catch
import System.Environment
import GHC.Stack

type API =
        "favicon.ico" :> Get '[ICO] ByteString
    :<|> "docs" :> Raw
    :<|> "static" :> Raw
    :<|> "jwt" :> JWT.Controller.API
    :<|> "oauth2" :> "client" :> OAuth2.Client.Controller.API
    :<|> "user" :> User.Controller.API
    :<|> "verification" :> Verification.Controller.API

class ( AuthenticationI p
      , JWTControllerI p
      , OAuth2ClientControllerI p
      , UserControllerI p
      , VerificationControllerI p
      , CORSI p
      , Utf8I p
      ) => PaperAppI p where
    server :: HasCallStack => Proxy p -> Context.Context -> FilePath -> FilePath -> Server API
    server = serverImpl
    faviconServer :: HasCallStack => Proxy p -> Context.Context -> Servant.Handler ByteString
    faviconServer = faviconServerImpl
    api :: Proxy p -> Proxy API
    api = apiImpl
    app :: HasCallStack => Proxy p -> Context.Context -> FilePath -> FilePath -> Application
    app = appImpl

serverImpl :: (HasCallStack, PaperAppI p) => Proxy p -> Context.Context -> FilePath -> FilePath -> Server API
serverImpl p context docsFilePath staticFilePath =
        faviconServer p context
    :<|> serveDirectoryWith (
        (defaultWebAppSettings docsFilePath) {
            ssMaxAge = NoMaxAge
          , ssUseHash = False
          })
    :<|> serveDirectoryWith ((defaultWebAppSettings staticFilePath) { ssMaxAge = NoMaxAge, ssUseHash = False })
    :<|> JWT.Controller.server p context
    :<|> OAuth2.Client.Controller.server p context
    :<|> User.Controller.server p context
    :<|> Verification.Controller.server p context

faviconServerImpl :: forall p. (HasCallStack, PaperAppI p) => Proxy p -> Context.Context -> Servant.Handler ByteString
faviconServerImpl profile context = do
    homeDir <- paperLog profile context $ getEnv "HOME"
    projectDir <- paperLog profile context $ Prelude.readFile $ homeDir ++ "/.paper-auth/project-directory"
    let filePath = projectDir ++ "resources/static/favicon.ico"
    Control.Monad.Catch.bracket
        (paperLog profile context $ openFile filePath ReadMode)
        (paperLog profile context . hClose)
        (paperLog profile context . Data.ByteString.hGetContents)

apiImpl :: PaperAppI p => Proxy p -> Proxy API
apiImpl _ = Proxy

appImpl :: (HasCallStack, PaperAppI p) => Proxy p -> Context.Context -> FilePath -> FilePath -> Application
appImpl p context docsFilePath staticFilePath = corsM p context $ utf8M p $ serveWithContext
    (api p)
    (authContext p context)
    (server p context docsFilePath staticFilePath)