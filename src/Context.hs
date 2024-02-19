module Context(
    Context(
        getConfig
      , getPaperAuthPool
    )
  , getContext'
) where

import Exception
import DB
import Paths_paper_auth

import Data.Configurator
import Data.Configurator.Types
import Control.Concurrent
import GHC.Stack

data Context = Context {
    getConfig :: Config
  , getPaperAuthPool :: PaperAuthPool
}

getConfig' :: HasCallStack => PaperExceptT IO (Config, ThreadId)
getConfig' = do
    filePath <- paperIO $ getDataFileName "resources/application.cfg"
    paperIO $ autoReload autoConfig [Required filePath]

getContext' :: HasCallStack => PaperExceptT IO Context
getContext' = do
    (config, _) <- getConfig'
    paperAuthPool <- getPaperAuthPool' config
    return $ Context {
        getConfig = config
      , getPaperAuthPool = paperAuthPool
    }