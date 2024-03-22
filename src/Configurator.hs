{-# LANGUAGE OverloadedStrings #-}

module Configurator(
    ConfiguratorI(
        lookupRequired
      , lookupRequiredGlobal
      , lookupRequiredNested
      )
) where

import CallStack
import GlobalMonad
import NestedMonad
import PaperMonad

import Servant
import Data.Configurator
import Data.Configurator.Types

import Control.Monad.IO.Unlift
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import GHC.Stack

class (PaperMonadI p, GlobalMonadI p, NestedMonadI p) => ConfiguratorI p where
    lookupRequired :: (HasCallStack, MonadUnliftIO m, Configured a) => Config -> Name -> PaperMonad p m a
    lookupRequired = lookupRequiredImpl
    lookupRequiredGlobal :: (HasCallStack, MonadUnliftIO m, Configured a) => Config -> Name -> GlobalMonad p m a
    lookupRequiredGlobal = lookupRequiredGlobalImpl
    lookupRequiredNested :: (HasCallStack, MonadUnliftIO m, Configured a) => Config -> Name -> NestedMonad p m a
    lookupRequiredNested = lookupRequiredNestedImpl

lookupRequiredImpl :: (HasCallStack, ConfiguratorI p, MonadUnliftIO m, Configured a) => Config -> Name -> PaperMonad p m a
lookupRequiredImpl config name = do
    profile <- ask
    maybeTToPaperMonadUnliftIO (MaybeT $ Data.Configurator.lookup config name) (PaperError ("configuration not found:\t" ++ show name) (err500 { errBody = "internal server error" }) (callStack' profile))

lookupRequiredGlobalImpl :: (HasCallStack, ConfiguratorI p, MonadUnliftIO m, Configured a) => Config -> Name -> GlobalMonad p m a
lookupRequiredGlobalImpl config name = do
    profile <- ask
    maybeTToGlobalMonadUnliftIO (MaybeT $ Data.Configurator.lookup config name) (GlobalError ("config missing:\t" ++ show name) (callStack' profile))

lookupRequiredNestedImpl :: (HasCallStack, ConfiguratorI p, MonadUnliftIO m, Configured a) => Config -> Name -> NestedMonad p m a
lookupRequiredNestedImpl config name = do
    profile <- ask
    maybeTToNestedMonadUnliftIO (MaybeT $ Data.Configurator.lookup config name) (NestedError ("config missing:\t" ++ show name) (callStack' profile))