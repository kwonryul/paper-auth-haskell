{-# LANGUAGE OverloadedStrings #-}

module Context(
    Context(
        config
      , paperAuthPool
      , paperEncodeSigner
      , paperVerifySigner
    )
  , ContextI(
        getContext
      )
) where

import DB
import GlobalMonad
import CallStack
import Import
import Paths_paper_auth

import Data.Configurator
import Data.Configurator.Types
import Web.JWT

import Control.Monad.IO.Unlift
import Control.Concurrent
import Data.ByteString
import Data.Proxy
import GHC.Stack

class DBI p => ContextI p where
    getConfig' :: (HasCallStack, MonadUnliftIO m) => GlobalMonad p m (Config, ThreadId)
    getConfig' = getConfig'Impl
    getContext :: (HasCallStack, MonadUnliftIO m) => GlobalMonad p m Context
    getContext = getContextImpl
    getPaperEncodeSigner' :: (HasCallStack, MonadUnliftIO m) => GlobalMonad p m EncodeSigner
    getPaperEncodeSigner' = getPaperEncodeSigner'Impl
    getPaperVerifySigner' :: (HasCallStack, MonadUnliftIO m) => GlobalMonad p m VerifySigner
    getPaperVerifySigner' = getPaperVerifySigner'Impl


getConfig'Impl :: (HasCallStack, ContextI p, MonadUnliftIO m) => GlobalMonad p m (Config, ThreadId)
getConfig'Impl = do
    filePath <- globalLiftIOUnliftIO $ getDataFileName "resources/application.cfg"
    globalLiftIOUnliftIO $ autoReload autoConfig [Required filePath]

getContextImpl :: (HasCallStack, ContextI p, MonadUnliftIO m) => GlobalMonad p m Context
getContextImpl = do
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

getPaperEncodeSigner'Impl :: forall p m. (HasCallStack, ContextI p, MonadUnliftIO m) => GlobalMonad p m EncodeSigner
getPaperEncodeSigner'Impl = do
    filePath <- globalLiftIOUnliftIO $ getDataFileName "resources/jwt/paper-auth.pem"
    content <- globalLiftIOUnliftIO $ Data.ByteString.readFile filePath
    case readRsaSecret content of
        Just privateKey -> return $ EncodeRSAPrivateKey privateKey
        Nothing -> toGlobalMonad $ GlobalError "paper-auth private key invalid" (callStack' profile)
    where
      profile :: Proxy p
      profile = Proxy

getPaperVerifySigner'Impl :: forall p m. (HasCallStack, ContextI p, MonadUnliftIO m) => GlobalMonad p m VerifySigner
getPaperVerifySigner'Impl = do
    filePath <- globalLiftIOUnliftIO $ getDataFileName "resources/jwt/paper-auth.pub"
    content <- globalLiftIOUnliftIO $ Data.ByteString.readFile filePath
    case readRsaPublicKey content of
        Just publicKey -> return $ VerifyRSAPublicKey publicKey
        Nothing -> toGlobalMonad $ GlobalError "paper-auth public key invalid" (callStack' profile)
    where
      profile :: Proxy p
      profile = Proxy