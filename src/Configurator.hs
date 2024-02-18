module Configurator(
    lookupConfig
) where

import Exception

import Data.Configurator as Cfg
import Data.Configurator.Types

import Data.Text

lookupConfig :: Configured a => Config -> Name -> PaperEitherT IO a
lookupConfig config name = toPaperEitherT' (Cfg.lookup config name) (ConfigMissing $ unpack name)