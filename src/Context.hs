{-# LANGUAGE OverloadedStrings #-}

module Context(
    Context(
        config
      , paperAuthPool
      , paperEncodeSigner
      , paperVerifySigner
      , oauth2ClientSocketConnections
    )
  , ContextI(
        getContext
      , getConfig'
      )
) where

import DB
import GlobalMonad
import CallStack
import Import

import Data.Configurator
import Data.Configurator.Types
import Web.JWT

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Concurrent
import Data.ByteString
import Data.Map
import System.Environment
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
    homeDir <- globalLiftIOUnliftIO $ getEnv "HOME"
    projectDir <- globalLiftIOUnliftIO $ Prelude.readFile $ homeDir ++ "/.paper-auth/project-directory"
    let filePath = projectDir ++ "resources/application.cfg"
    globalLiftIOUnliftIO $ autoReload autoConfig [Required filePath]

getContextImpl :: (HasCallStack, ContextI p, MonadUnliftIO m) => GlobalMonad p m Context
getContextImpl = do
    (config, _) <- getConfig'
    paperAuthPool <- getPaperAuthPool' config
    paperEncodeSigner <- getPaperEncodeSigner'
    paperVerifySigner <- getPaperVerifySigner'
    oauth2ClientSocketConnections <- globalLiftIOUnliftIO $ newMVar Data.Map.empty
    return $ Context {
        config
      , paperAuthPool
      , paperEncodeSigner
      , paperVerifySigner
      , oauth2ClientSocketConnections
    }

getPaperEncodeSigner'Impl :: (HasCallStack, ContextI p, MonadUnliftIO m) => GlobalMonad p m EncodeSigner
getPaperEncodeSigner'Impl = do
    profile <- ask
    homeDir <- globalLiftIOUnliftIO $ getEnv "HOME"
    projectDir <- globalLiftIOUnliftIO $ Prelude.readFile $ homeDir ++ "/.paper-auth/project-directory"
    let filePath = projectDir ++ "resources/jwt/paper-auth.pem"
    content <- globalLiftIOUnliftIO $ Data.ByteString.readFile filePath
    case readRsaSecret content of
        Just privateKey -> return $ EncodeRSAPrivateKey privateKey
        Nothing -> toGlobalMonad $ GlobalError "paper-auth private key invalid" (callStack' profile)

getPaperVerifySigner'Impl :: (HasCallStack, ContextI p, MonadUnliftIO m) => GlobalMonad p m VerifySigner
getPaperVerifySigner'Impl = do
    profile <- ask
    homeDir <- globalLiftIOUnliftIO $ getEnv "HOME"
    projectDir <- globalLiftIOUnliftIO $ Prelude.readFile $ homeDir ++ "/.paper-auth/project-directory"
    let filePath = projectDir ++ "resources/jwt/paper-auth.pub"
    content <- globalLiftIOUnliftIO $ Data.ByteString.readFile filePath
    case readRsaPublicKey content of
        Just publicKey -> return $ VerifyRSAPublicKey publicKey
        Nothing -> toGlobalMonad $ GlobalError "paper-auth public key invalid" (callStack' profile)