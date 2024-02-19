module Configurator(
    lookupConfig
) where

import Exception
import CallStack

import Data.Configurator as Cfg
import Data.Configurator.Types

import Data.Text
import GHC.Stack

lookupConfig :: (HasCallStack, Configured a) => Config -> Name -> PaperExceptT IO a
lookupConfig config name = maybeToPaperExceptT' (Cfg.lookup config name) (ConfigMissing (unpack name) callStack')