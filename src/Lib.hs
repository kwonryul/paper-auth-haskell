{-# LANGUAGE DataKinds #-}

module Lib (startApp) where

import Context as C
import Paths_paper

import Servant
import Servant.Static.TH.Internal.Mime
import Network.Wai.Handler.Warp

import System.IO
import Data.ByteString as BS
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Exception hiding (
    Handler
  , handle
  )

type API = "favicon.ico" :> Get '[ICO] ByteString

server :: C.Context -> FilePath -> Server API
server context filePath = faviconServer

faviconServer :: Handler ByteString
faviconServer = liftIO $ do
    filePath <- getDataFileName "resources/images/favicon.ico"
    handle <- openFile filePath ReadMode
    contents <- BS.hGetContents handle
    hClose handle
    return contents

api :: Proxy API
api = Proxy

app :: C.Context -> FilePath -> Application
app context filePath = serve api (server context filePath)

startApp :: IO ()
startApp = do
    filePath <- getDataFileName "resources/static"
    context' <- runStderrLoggingT $ runEitherT getContext'
    case context' of
        Right context -> run 8080 (app context filePath)
        Left ex -> throwIO ex