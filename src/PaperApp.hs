{-# LANGUAGE DataKinds #-}

module PaperApp (
    PaperAppI(
        app
      )
  ) where

import qualified JWT.Controller
import JWT.Controller (JWTControllerI)
import qualified User.Controller
import User.Controller (UserControllerI)

import Context
import Authentication
import PaperMonad
import Paths_paper_auth

import Servant
import Servant.Static.TH.Internal.Mime

import System.IO
import Data.ByteString
import Control.Monad.Catch
import GHC.Stack

type API = "favicon.ico" :> Get '[ICO] ByteString
    :<|> "static" :> Raw
    :<|> "jwt" :> JWT.Controller.API
--    :<|> "oauth2" :> OAuth2.API
    :<|> "user" :> User.Controller.API

class (AuthenticationI p, UserControllerI p, JWTControllerI p) => PaperAppI p where
    server :: HasCallStack => Proxy p -> Context.Context -> FilePath -> Server API
    server = serverImpl
    faviconServer :: HasCallStack => Proxy p -> Context.Context -> Servant.Handler ByteString
    faviconServer = faviconServerImpl
    api :: Proxy p -> Proxy API
    api = apiImpl
    app :: HasCallStack => Proxy p -> Context.Context -> FilePath -> Application
    app = appImpl

serverImpl :: (HasCallStack, PaperAppI p) => Proxy p -> Context.Context -> FilePath -> Server API
serverImpl p context staticFilePath = faviconServer p context
    :<|> serveDirectoryWebApp staticFilePath
    :<|> JWT.Controller.server p context
--    :<|> (OAuth2.server context)
    :<|> User.Controller.server p context

faviconServerImpl :: (HasCallStack, PaperAppI p) => Proxy p -> Context.Context -> Servant.Handler ByteString
faviconServerImpl profile context = do
    filePath <- paperLog profile context $ getDataFileName "resources/images/favicon.ico"
    Control.Monad.Catch.bracket
        (paperLog profile context $ openFile filePath ReadMode)
        (paperLog profile context . hClose)
        (paperLog profile context . Data.ByteString.hGetContents)

apiImpl :: PaperAppI p => Proxy p -> Proxy API
apiImpl _ = Proxy

appImpl :: (HasCallStack, PaperAppI p) => Proxy p -> Context.Context -> FilePath -> Application
appImpl p context staticFilePath = serveWithContext
    (api p)
    (authContext p context)
    (server p context staticFilePath)