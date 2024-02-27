{-# LANGUAGE OverloadedStrings #-}

module Configurator(
    lookupConfig
  , lookupConfigGlobal
) where

import PaperError
import GlobalError
import CallStack

import Servant

import Data.Configurator
import Data.Configurator.Types

import Control.Monad.Trans.Maybe
import GHC.Stack

lookupConfig :: (HasCallStack, Configured a) => Config -> Name -> PaperExceptT IO a
lookupConfig config name = maybeTToPaperExceptT (MaybeT $ Data.Configurator.lookup config name) (PaperException ("configuration not found:\t" ++ show name) (err500 { errBody = "Internal server error" }) callStack')

lookupConfigGlobal :: (HasCallStack, Configured a) => Config -> Name -> GlobalExceptT IO a
lookupConfigGlobal config name = maybeTToGlobalExceptT (MaybeT $ Data.Configurator.lookup config name) (GlobalException ("config missing:\t" ++ show name) callStack')