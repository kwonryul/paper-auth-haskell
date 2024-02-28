{-# LANGUAGE OverloadedStrings #-}

module Configurator(
    lookupRequired
  , lookupRequiredGlobal
) where

import PaperError
import GlobalError
import CallStack

import Servant

import Data.Configurator
import Data.Configurator.Types

import Control.Monad.IO.Unlift
import Control.Monad.Trans.Maybe
import GHC.Stack

lookupRequired :: (HasCallStack, MonadUnliftIO m, Configured a) => Config -> Name -> PaperExceptT m a
lookupRequired config name = maybeTToPaperExceptT (MaybeT $ Data.Configurator.lookup config name) (PaperException ("configuration not found:\t" ++ show name) (err500 { errBody = "Internal server error" }) callStack')

lookupRequiredGlobal :: (HasCallStack, MonadUnliftIO m, Configured a) => Config -> Name -> GlobalExceptT m a
lookupRequiredGlobal config name = maybeTToGlobalExceptT (MaybeT $ Data.Configurator.lookup config name) (GlobalException ("config missing:\t" ++ show name) callStack')