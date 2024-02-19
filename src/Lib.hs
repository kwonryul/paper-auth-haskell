{-# LANGUAGE DataKinds #-}

module Lib (startApp) where

import Context as C
import Exception
import Paths_paper_auth

import Servant
import Servant.Static.TH.Internal.Mime
import Network.Wai.Handler.Warp

import System.IO
import Data.ByteString as BS
import Control.Monad.IO.Class
import GHC.Stack

type API = "favicon.ico" :> Get '[ICO] ByteString

server :: HasCallStack => C.Context -> FilePath -> Server API
server context filePath = faviconServer

faviconServer :: HasCallStack => Handler ByteString
faviconServer = liftIO $ do
    filePath <- paperIO' $ getDataFileName "resources/images/favicon.ico"
    handle <- paperIO' $ openFile filePath ReadMode
    contents <- paperIO' $ BS.hGetContents handle
    paperIO' $ hClose handle
    return contents

api :: Proxy API
api = Proxy

app :: HasCallStack => C.Context -> FilePath -> Application
app context filePath = serve api (server context filePath)

startApp :: HasCallStack => IO ()
startApp = do
    filePath <- paperIO' $ getDataFileName "resources/static"
    context <- runPaperExceptT getContext'
    paperIO' $ run 8080 (app context filePath)