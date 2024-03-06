module Main where

import Lib
import Profile.Prod

import Data.Proxy
import GHC.Stack

profile :: Proxy Prod
profile = Proxy

main :: HasCallStack => IO ()
main = do
    Resources {
        docsFilePath
      , staticFilePath
      , context
      , certPath
      , secretKeyPath
      } <- getAllResources profile
    migratePaperAuth profile context (paperAuthPool context)
    startApp profile docsFilePath staticFilePath context certPath secretKeyPath