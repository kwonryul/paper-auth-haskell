module Main where

import Lib
import Profile.Prod
import Profile.Test.Import
import Profile.Test()

import Control.Concurrent
import Data.Proxy
import GHC.Stack

prod :: Proxy Prod
prod = Proxy

test :: Proxy Test
test = Proxy

main :: HasCallStack => IO ()
main = do
    Resources {
        context = prodContext
      , certPath = prodCertPath
      , secretKeyPath = prodSecretKeyPath
      } <- getAllResources prod
    Resources {
        context = testContext
      , certPath = testCertPath
      , secretKeyPath = testSecretKeyPath
      } <- getAllResources test
    migratePaperAuth prod prodContext (paperAuthPool prodContext)
    migratePaperAuth test testContext (paperAuthPool testContext)
    _ <- forkIO $ startApp test testContext testCertPath testSecretKeyPath
    startApp prod prodContext prodCertPath prodSecretKeyPath