module Main where

import Lib
import Context
import GlobalError

import GHC.Stack

main :: HasCallStack => IO ()
main = do
    runGlobalExceptT $ do
        Resources {
            staticFilePath
          , context
          , certPath
          , secretKeyPath
        } <- getAllResources
        migratePaperAuth (paperAuthPool context)
        startApp staticFilePath context certPath secretKeyPath