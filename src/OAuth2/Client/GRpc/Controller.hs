module OAuth2.Client.GRpc.Controller(
    sendTokenAndCloseHs
) where

import OAuth2.Client.GRpc.DTO
import OAuth2.Client.Model
import Import

import Network.WebSockets
import Data.Aeson

import Foreign.C.String

import Control.Concurrent.MVar
import Control.Exception
import Data.Map
import Data.Text

sendTokenAndCloseHs :: OAuth2ClientSocketConnections -> Int -> CString -> CString -> IO CString
sendTokenAndCloseHs socketConnections' socketId accessToken' refreshToken'' = do
    accessToken <- Data.Text.pack <$> peekCString accessToken'
    refreshToken' <- peekCString refreshToken''
    let refreshToken = if refreshToken' == "" then Nothing else Just $ Data.Text.pack refreshToken'
    socketConnections <- readMVar socketConnections'
    case Data.Map.lookup socketId socketConnections of
        Just (OAuth2ClientWebSocketConnection {
            connection
          , sendLock
          , closeShot
        }) -> do
            bracket (takeMVar sendLock) (\_ -> putMVar sendLock ()) (\_ ->
                sendTextData connection $ encode $ IssueJWTResDTO accessToken refreshToken
                )
            putMVar closeShot ()
            ok <- newCString "OK"
            return ok
        Just (OAuth2ClientNativeSocketConnection {
        }) -> undefined
        Nothing -> do
            err <- newCString "socket closed or invalid"
            return err