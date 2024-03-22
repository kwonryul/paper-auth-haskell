{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module OAuth2.Client.GRpc.ExService(
    OAuth2ClientGRpcExServiceI(
        sendToken
      )
) where

import CallStack
import Import
import NestedMonad
import PaperMonad

import Servant
import Data.Configurator.Types

import Foreign.C.String
import Foreign.Marshal.Alloc

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Exception
import Data.Text
import GHC.Stack

foreign import ccall "send_token_and_close_c" sendTokenAndCloseC :: CString -> Int -> Int -> CString -> CString -> IO CString

class (NestedMonadI p, PaperMonadI p) => OAuth2ClientGRpcExServiceI p where
    sendToken :: (HasCallStack, MonadUnliftIO m) => Config -> String -> Int -> OAuth2ClientSocketId' -> Text -> Maybe Text -> PaperMonad p m ()
    sendToken = sendTokenImpl

sendTokenImpl :: forall p m. (HasCallStack, OAuth2ClientGRpcExServiceI p, MonadUnliftIO m) => Config -> String -> Int -> OAuth2ClientSocketId' -> Text -> Maybe Text -> PaperMonad p m ()
sendTokenImpl cfg host port socketId accessToken refreshToken = do
    profile <- ask
    res <- paperLiftIOUnliftIO $ bracket (
        bracket (nestedLog profile cfg $ newCString host) (nestedLog profile cfg . free) (\h ->
            bracket (nestedLog profile cfg $ newCString $ Data.Text.unpack accessToken) (nestedLog profile cfg . free) (\at ->
                bracket (nestedLog profile cfg $ newCString $ maybe "" Data.Text.unpack refreshToken) (nestedLog profile cfg . free) (\rt ->
                    nestedLog profile cfg $ sendTokenAndCloseC h port socketId at rt
                    )
                )
            )
        ) (nestedLog profile cfg . free) (nestedLog profile cfg . peekCString)
    case res of
        "OK" ->
            return ()
        err ->
            toPaperMonad $ PaperError ("GRpc error\n" ++ err) (err500 { errBody = "internal server error" }) $ callStack' profile