{-# LANGUAGE OverloadedStrings #-}

module Context(
    Context(
        getConfig
      , getPaperAuthPool
      , getPaperEncodeSigner
      , getPaperVerifySigner
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
import Data.ByteString as ByteString
import GHC.Stack

data Context = Context {
    getConfig :: Config
  , getPaperAuthPool :: PaperAuthPool
  , getPaperEncodeSigner :: EncodeSigner
  , getPaperVerifySigner :: VerifySigner
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
        getConfig = config
      , getPaperAuthPool = paperAuthPool
      , getPaperEncodeSigner = paperEncodeSigner
      , getPaperVerifySigner = paperVerifySigner
    }

getPaperEncodeSigner' :: HasCallStack => GlobalExceptT IO EncodeSigner
getPaperEncodeSigner' = do
    filePath <- globalLiftIO $ getDataFileName "resources/jwt/paper.pem"
    content <- globalLiftIO $ ByteString.readFile filePath
    case readRsaSecret content of
        Just privateKey -> return $ EncodeRSAPrivateKey privateKey
        Nothing -> toGlobalExceptT $ GlobalException "paper private key invalid" callStack'

getPaperVerifySigner' :: HasCallStack => GlobalExceptT IO VerifySigner
getPaperVerifySigner' = do
    filePath <- globalLiftIO $ getDataFileName "resources/jwt/paper.pub"
    content <- globalLiftIO $ ByteString.readFile filePath
    case readRsaPublicKey content of
        Just publicKey -> return $ VerifyRSAPublicKey publicKey
        Nothing -> toGlobalExceptT $ GlobalException "paper public key invalid" callStack'