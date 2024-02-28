{-# LANGUAGE DataKinds #-}

module PaperApp (app) where

import qualified JWT.Controller
import qualified User.Controller

import Context
import Authentication
import PaperError
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

server :: HasCallStack => Context.Context -> FilePath -> Server API
server context filePath = faviconServer
    :<|> serveDirectoryWebApp filePath
    :<|> JWT.Controller.server context
--    :<|> (OAuth2.server context)
    :<|> User.Controller.server context

faviconServer :: HasCallStack => Servant.Handler ByteString
faviconServer = do
    filePath <- paperLog $ getDataFileName "resources/images/favicon.ico"
    Control.Monad.Catch.bracket
        (paperLog $ openFile filePath ReadMode)
        (paperLog . hClose)
        (paperLog . Data.ByteString.hGetContents)

api :: Proxy API
api = Proxy

app :: HasCallStack => Context.Context -> FilePath -> Application
app context filePath = serveWithContext
    api
    (authContext (paperAuthPool context) (paperVerifySigner context))
    (server context filePath)