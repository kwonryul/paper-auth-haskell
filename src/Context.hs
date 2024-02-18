module Context(
    Context
  , getContext'
) where

import Exception
import DB
import Paths_paper

import Data.Configurator
import Data.Configurator.Types
import Control.Concurrent
import Control.Monad.Trans.Resource
import Control.Monad.Logger

data Context = Context {
    getConfig :: Config
  , getPaperAuthPool :: PaperAuthPool
}

getConfig' :: IO (Config, ThreadId)
getConfig' = do
    filePath <- getDataFileName "resources/application.cfg"
    autoReload autoConfig [Required filePath]

getContext' :: (MonadUnliftIO m, MonadLoggerIO m) => PaperEitherT m Context
getContext' = do
    (config, _) <- liftIOEitherT' $ Right <$> getConfig'
    paperAuthPool <- getPaperAuthPool' config
    return $ Context {
        getConfig = config
      , getPaperAuthPool = paperAuthPool
    }