{-# LANGUAGE DataKinds #-}

module Lib (startApp) where

import Context
import JWT.AuthCheck
import GlobalError
import PaperError
import Paths_paper_auth

import qualified JWT.Controller as JWT

import Servant
import Servant.Static.TH.Internal.Mime
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import System.IO
import Data.ByteString as BS
import Control.Monad.Catch as Catch
import Control.Concurrent
import GHC.Stack

type API = "favicon.ico" :> Get '[ICO] ByteString
    :<|> "static" :> Raw
    :<|> "jwt" :> JWT.API
--    :<|> "oauth2" :> OAuth2.API

server :: HasCallStack => Context.Context -> FilePath -> Server API
server context filePath = faviconServer
    :<|> serveDirectoryWebApp filePath
    :<|> (JWT.server context)
--    :<|> (OAuth2.server context)

faviconServer :: HasCallStack => Servant.Handler ByteString
faviconServer = do
    filePath <- paperLog $ getDataFileName "resources/images/favicon.ico"
    Catch.bracket
        (paperLog $ openFile filePath ReadMode)
        (paperLog . hClose)
        (paperLog . BS.hGetContents)

api :: Proxy API
api = Proxy

app :: HasCallStack => Context.Context -> FilePath -> Application
app context filePath = serveWithContext
    api
    (authContext $ getPaperAuthPool context)
    (server context filePath)

startApp :: HasCallStack => IO ()
startApp = do
    filePath <- globalLog $ getDataFileName "resources/static"
    context <- runGlobalExceptT getContext'
    certPath <- globalLog $ getDataFileName "resources/tls/cert.pem"
    secretKeyPath <- globalLog $ getDataFileName "resources/tls/secret-key.pem"
    _ <- forkIO $ (globalLog $ run 80 (app context filePath))
    globalLog $ runTLS
        (tlsSettings certPath secretKeyPath)
        (setPort 443 defaultSettings)
        (app context filePath)