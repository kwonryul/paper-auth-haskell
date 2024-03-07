{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Profile.Test.Import
import Profile.Test()
import Test.JWT
import Test.User

import Test.Hspec

import Data.Proxy

profile :: Proxy Test
profile = Proxy

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = do
    jwtSpec profile
    userSpec profile

{-
main :: HasCallStack => IO ()
main = do
    Resources {
        Lib.context = ctx
      , certPath
      , secretKeyPath
      } <- getAllResources profile
    migratePaperAuth profile ctx (paperAuthPool ctx)
    startApp profile ctx certPath secretKeyPath
-}

{-}
main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  with (do
    Resources {
        staticFilePath
      , Lib.context = ctx
      } <- getAllResources profile
    return $ app profile ctx staticFilePath) $ do
        describe "GET /users" $ do
          it "responds with 200" $ do
              get "/users" `shouldRespondWith` 200
          it "responds with [User]" $ do
              let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
              get "/users" `shouldRespondWith` users
-}