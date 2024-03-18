{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module OAuth2.Client.GRpc.ExService(
    OAuth2ClientGRpcExServiceI(
        sendToken
      )
) where

import CallStack
import Import
import PaperMonad

import Servant

import Foreign.C.String
import Foreign.Marshal.Alloc

import Control.Monad.IO.Unlift
import Control.Exception
import Data.Text
import GHC.Stack

foreign import ccall "send_token_and_close_c" sendTokenAndCloseC :: CString -> Int -> Int -> CString -> CString -> IO CString

class PaperMonadI p => OAuth2ClientGRpcExServiceI p where
    sendToken :: (HasCallStack, MonadUnliftIO m) => String -> Int -> OAuth2ClientSocketId' -> Text -> Maybe Text -> PaperMonad p m ()
    sendToken = sendTokenImpl

sendTokenImpl :: forall p m. (HasCallStack, OAuth2ClientGRpcExServiceI p, MonadUnliftIO m) => String -> Int -> OAuth2ClientSocketId' -> Text -> Maybe Text -> PaperMonad p m ()
sendTokenImpl host port socketId accessToken refreshToken = do
    res <- paperLiftIOUnliftIO $ bracket (
        bracket (newCString host) free (\h ->
            bracket (newCString $ Data.Text.unpack accessToken) free (\at ->
                bracket (newCString $ maybe "" Data.Text.unpack refreshToken) free (\rt ->
                    sendTokenAndCloseC h port socketId at rt
                    )
                )
            )
        ) free peekCString
    case res of
        "OK" ->
            return ()
        err ->
            toPaperMonad $ PaperError ("GRpc error\n" ++ err) (err500 { errBody = "internal server error" }) $ callStack' profile
    where
        profile :: Proxy p
        profile = Proxy