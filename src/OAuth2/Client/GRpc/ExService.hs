{-# LANGUAGE ForeignFunctionInterface #-}

module OAuth2.Client.GRpc.ExService(
    OAuth2ClientGRpcExServiceI(
        sendToken
      )
) where

import Import
import PaperMonad

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

sendTokenImpl :: (HasCallStack, OAuth2ClientGRpcExServiceI p, MonadUnliftIO m) => String -> Int -> OAuth2ClientSocketId' -> Text -> Maybe Text -> PaperMonad p m ()
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
        "OK" -> do
            paperLiftIOUnliftIO $ print "test - ok"
            return ()
        "FAILED" -> do
            paperLiftIOUnliftIO $ print "test - ok"
            return ()
        x -> do
            paperLiftIOUnliftIO $ print $ "test - " ++ x
            return ()