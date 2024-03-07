{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PaperApp (
    PaperAppI(
        app
      )
#ifdef TEST
  , appImpl
#endif
  ) where

import qualified JWT.Controller
import JWT.Controller (JWTControllerI)
import qualified User.Controller
import User.Controller (UserControllerI)

import Configurator
import Context
import Authentication
import PaperMonad

import Servant
import Servant.Static.TH.Internal.Mime

import System.IO
import Data.ByteString
import Control.Monad.Catch
import GHC.Stack

type API = "favicon.ico" :> Get '[ICO] ByteString
    :<|> "docs" :> Raw
    :<|> "static" :> Raw
    :<|> "jwt" :> JWT.Controller.API
--    :<|> "oauth2" :> OAuth2.API
    :<|> "user" :> User.Controller.API

class (AuthenticationI p, UserControllerI p, JWTControllerI p) => PaperAppI p where
    server :: HasCallStack => Proxy p -> Context.Context -> FilePath -> FilePath -> Server API
    server = serverImpl
    faviconServer :: HasCallStack => Proxy p -> Context.Context -> Servant.Handler ByteString
    faviconServer = faviconServerImpl
    api :: Proxy p -> Proxy API
    api = apiImpl
    app :: HasCallStack => Proxy p -> Context.Context -> FilePath -> FilePath -> Application
    app = appImpl

serverImpl :: (HasCallStack, PaperAppI p) => Proxy p -> Context.Context -> FilePath -> FilePath -> Server API
serverImpl p context docsFilePath staticFilePath = faviconServer p context
    :<|> serveDirectoryWebApp docsFilePath
    :<|> serveDirectoryWebApp staticFilePath
    :<|> JWT.Controller.server p context
--    :<|> (OAuth2.server context)
    :<|> User.Controller.server p context

faviconServerImpl :: forall p. (HasCallStack, PaperAppI p) => Proxy p -> Context.Context -> Servant.Handler ByteString
faviconServerImpl profile context = do
    projectDir <- runPaperMonad @p context $ lookupRequired (config context) "projectDir"
    let filePath = projectDir ++ "static/favicon.ico"
    Control.Monad.Catch.bracket
        (paperLog profile context $ openFile filePath ReadMode)
        (paperLog profile context . hClose)
        (paperLog profile context . Data.ByteString.hGetContents)

apiImpl :: PaperAppI p => Proxy p -> Proxy API
apiImpl _ = Proxy

appImpl :: (HasCallStack, PaperAppI p) => Proxy p -> Context.Context -> FilePath -> FilePath -> Application
appImpl p context docsFilePath staticFilePath = serveWithContext
    (api p)
    (authContext p context)
    (server p context docsFilePath staticFilePath)