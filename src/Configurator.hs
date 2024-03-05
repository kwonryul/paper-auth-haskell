{-# LANGUAGE OverloadedStrings #-}

module Configurator(
    ConfiguratorI(
        lookupRequired
      , lookupRequiredGlobal
      )
) where

import PaperMonad
import GlobalMonad
import CallStack

import Servant
import Data.Configurator
import Data.Configurator.Types

import Control.Monad.IO.Unlift
import Control.Monad.Trans.Maybe
import GHC.Stack

class (PaperMonadI p, GlobalMonadI p) => ConfiguratorI p where
    lookupRequired :: (HasCallStack, MonadUnliftIO m, Configured a) => Config -> Name -> PaperMonad p m a
    lookupRequired = lookupRequiredImpl
    lookupRequiredGlobal :: (HasCallStack, MonadUnliftIO m, Configured a) => Config -> Name -> GlobalMonad p m a
    lookupRequiredGlobal = lookupRequiredGlobalImpl

lookupRequiredImpl :: forall p m a. (HasCallStack, ConfiguratorI p, MonadUnliftIO m, Configured a) => Config -> Name -> PaperMonad p m a
lookupRequiredImpl config name =
    maybeTToPaperMonadUnliftIO (MaybeT $ Data.Configurator.lookup config name) (PaperError ("configuration not found:\t" ++ show name) (err500 { errBody = "Internal server error" }) (callStack' profile))
    where
        profile :: Proxy p
        profile = Proxy

lookupRequiredGlobalImpl :: forall p m a. (HasCallStack, ConfiguratorI p, MonadUnliftIO m, Configured a) => Config -> Name -> GlobalMonad p m a
lookupRequiredGlobalImpl config name =
    maybeTToGlobalMonadUnliftIO (MaybeT $ Data.Configurator.lookup config name) (GlobalError ("config missing:\t" ++ show name) (callStack' profile))
    where
        profile :: Proxy p
        profile = Proxy