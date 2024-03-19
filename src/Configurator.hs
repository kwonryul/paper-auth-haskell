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
import GHC.Stack

class (PaperMonadI p, GlobalMonadI p, NestedMonadI p) => ConfiguratorI p where
    lookupRequired :: (HasCallStack, MonadUnliftIO m, Configured a) => Config -> Name -> PaperMonad p m a
    lookupRequired = lookupRequiredImpl
    lookupRequiredGlobal :: (HasCallStack, MonadUnliftIO m, Configured a) => Config -> Name -> GlobalMonad p m a
    lookupRequiredGlobal = lookupRequiredGlobalImpl
    lookupRequiredNested :: (HasCallStack, MonadUnliftIO m, Configured a) => Config -> Name -> NestedMonad p m a
    lookupRequiredNested = lookupRequiredNestedImpl

lookupRequiredImpl :: forall p m a. (HasCallStack, ConfiguratorI p, MonadUnliftIO m, Configured a) => Config -> Name -> PaperMonad p m a
lookupRequiredImpl config name =
    maybeTToPaperMonadUnliftIO (MaybeT $ Data.Configurator.lookup config name) (PaperError ("configuration not found:\t" ++ show name) (err500 { errBody = "internal server error" }) (callStack' profile))
    where
        profile :: Proxy p
        profile = Proxy

lookupRequiredGlobalImpl :: forall p m a. (HasCallStack, ConfiguratorI p, MonadUnliftIO m, Configured a) => Config -> Name -> GlobalMonad p m a
lookupRequiredGlobalImpl config name =
    maybeTToGlobalMonadUnliftIO (MaybeT $ Data.Configurator.lookup config name) (GlobalError ("config missing:\t" ++ show name) (callStack' profile))
    where
        profile :: Proxy p
        profile = Proxy

lookupRequiredNestedImpl :: forall p m a. (HasCallStack, ConfiguratorI p, MonadUnliftIO m, Configured a) => Config -> Name -> NestedMonad p m a
lookupRequiredNestedImpl config name =
    maybeTToNestedMonadUnliftIO (MaybeT $ Data.Configurator.lookup config name) (NestedError ("config missing:\t" ++ show name) (callStack' profile))
    where
        profile :: Proxy p
        profile = Proxy