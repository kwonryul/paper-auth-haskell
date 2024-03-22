module OAuth2.Client.GRpc.Controller(
    OAuth2ClientGRpcControllerI(
        sendTokenAndCloseHs
      )
) where

import OAuth2.Client.GRpc.DTO
import OAuth2.Client.Model
import Import
import NestedMonad

import Network.WebSockets
import Data.Aeson
import Data.Configurator.Types

import Foreign.C.String

import Control.Concurrent.MVar
import Control.Exception
import Data.Map
import Data.Text
import Data.Proxy
import GHC.Stack

class NestedMonadI p => OAuth2ClientGRpcControllerI p where
    sendTokenAndCloseHs :: HasCallStack => Proxy p -> Config -> OAuth2ClientSocketConnections -> Int -> CString -> CString -> IO CString
    sendTokenAndCloseHs = sendTokenAndCloseHsImpl


sendTokenAndCloseHsImpl :: (HasCallStack, NestedMonadI p) => Proxy p -> Config -> OAuth2ClientSocketConnections -> Int -> CString -> CString -> IO CString
sendTokenAndCloseHsImpl profile cfg socketConnections' socketId accessToken' refreshToken'' = do
    accessToken <- nestedLog profile cfg $ Data.Text.pack <$> peekCString accessToken'
    refreshToken' <- nestedLog profile cfg $ peekCString refreshToken''
    let refreshToken = if refreshToken' == "" then Nothing else Just $ Data.Text.pack refreshToken'
    socketConnections <- nestedLog profile cfg $ readMVar socketConnections'
    case Data.Map.lookup socketId socketConnections of
        Just (OAuth2ClientWebSocketConnection {
            connection
          , sendLock
          , closeShot
        }) -> do
            bracket (nestedLog profile cfg $ takeMVar sendLock) (\_ -> nestedLog profile cfg $ putMVar sendLock ()) (\_ ->
                nestedLog profile cfg $ sendTextData connection $ encode $ IssueJWTResDTO accessToken refreshToken
                )
            nestedLog profile cfg $ putMVar closeShot ()
            ok <- nestedLog profile cfg $ newCString "OK"
            return ok
        Just (OAuth2ClientNativeSocketConnection {
        }) -> undefined
        Nothing -> do
            err <- nestedLog profile cfg $ newCString "socket closed or invalid"
            return err