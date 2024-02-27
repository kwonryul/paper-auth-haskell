{-# LANGUAGE OverloadedStrings #-}

module Context(
    Context(
        config
      , paperAuthPool
      , paperEncodeSigner
      , paperVerifySigner
    )
  , getContext'
) where

import GlobalError
import DB
import CallStack
import Paths_paper_auth

import Data.Configurator
import Data.Configurator.Types
import Web.JWT

import Control.Concurrent
import Data.ByteString
import GHC.Stack

data Context = Context {
    config :: Config
  , paperAuthPool :: PaperAuthPool
  , paperEncodeSigner :: EncodeSigner
  , paperVerifySigner :: VerifySigner
}

getConfig' :: HasCallStack => GlobalExceptT IO (Config, ThreadId)
getConfig' = do
    filePath <- globalLiftIO $ getDataFileName "resources/application.cfg"
    globalLiftIO $ autoReload autoConfig [Required filePath]

getContext' :: HasCallStack => GlobalExceptT IO Context
getContext' = do
    (config, _) <- getConfig'
    paperAuthPool <- getPaperAuthPool' config
    paperEncodeSigner <- getPaperEncodeSigner'
    paperVerifySigner <- getPaperVerifySigner'
    return $ Context {
        config = config
      , paperAuthPool = paperAuthPool
      , paperEncodeSigner = paperEncodeSigner
      , paperVerifySigner = paperVerifySigner
    }

getPaperEncodeSigner' :: HasCallStack => GlobalExceptT IO EncodeSigner
getPaperEncodeSigner' = do
    filePath <- globalLiftIO $ getDataFileName "resources/jwt/paper-auth.pem"
    content <- globalLiftIO $ Data.ByteString.readFile filePath
    case readRsaSecret content of
        Just privateKey -> return $ EncodeRSAPrivateKey privateKey
        Nothing -> toGlobalExceptT $ GlobalException "paper-auth private key invalid" callStack'

getPaperVerifySigner' :: HasCallStack => GlobalExceptT IO VerifySigner
getPaperVerifySigner' = do
    filePath <- globalLiftIO $ getDataFileName "resources/jwt/paper-auth.pub"
    content <- globalLiftIO $ Data.ByteString.readFile filePath
    case readRsaPublicKey content of
        Just publicKey -> return $ VerifyRSAPublicKey publicKey
        Nothing -> toGlobalExceptT $ GlobalException "paper-auth public key invalid" callStack'