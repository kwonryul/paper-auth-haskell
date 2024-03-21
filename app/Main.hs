{-# LANGUAGE LambdaCase #-}

module Main where

import Profile.Test.Import
import Profile.Dev
import Profile.Prod
import Profile.Test()
import Lib

import Control.Concurrent
import Control.Exception
import Data.Proxy
import System.Environment
import GHC.Stack

prod :: Proxy Prod
prod = Proxy

dev :: Proxy Dev
dev = Proxy

test :: Proxy Test
test = Proxy

main :: HasCallStack => IO ()
main = do
    args <- getArgs
    execute $ (\case
        "prod" -> main' prod
        "dev" -> main' dev
        "test" -> main' test
        _ -> throwIO $ userError "argument invalid"
        ) <$> args
    where
        execute :: [IO ()] -> IO ()
        execute [] = return ()
        execute [x] = x
        execute (x : xs) = forkIO x >> execute xs

main' :: (HasCallStack, LibI p) => Proxy p -> IO ()
main' p = do
    Resources {
        context
      , certPath
      , secretKeyPath
      } <- getAllResources p
    migratePaperAuth p context (paperAuthPool context)
    startApp p context certPath secretKeyPath