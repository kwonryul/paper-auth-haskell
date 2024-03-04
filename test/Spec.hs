{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Monad.ErrorT
import PaperMonad
import CallStack
import Data.Proxy
import Servant
import Control.Exception
import UnliftIO.Exception
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Control.Monad.Catch

main :: IO ()
main = undefined
{-}
import Lib

import GHC.Stack

import Test.Hspec
import Test.Hspec.Wai

import Verification.Repository
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  with (do
    Resources {
        staticFilePath
      , Lib.context
      , certPath
      , secretKeyPath
      } <- getAllResources
    return $ app context staticFilePath) $ do
        describe "GET /users" $ do
          it "responds with 200" $ do
              get "/users" `shouldRespondWith` 200
          it "responds with [User]" $ do
              let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
              get "/users" `shouldRespondWith` users
-}

data Crola = Crola deriving Show
instance Exception Crola

moka :: ErrorT PaperErrorP IO ()
moka = unSafeErrorT $ toSafeErrorT (Proxy :: Proxy PaperError) $ toInnerError $ PaperError "mokamoka" (err500 { errBody = "errobydo = moka" }) callStack'

mola :: IO ()
mola = do
    let ErrorT (LoggingT f) = Control.Monad.Catch.catch moka (\(_ :: SomeException) ->
          liftIO $ print "mimi"
          )
    let ExceptT m = f (\_ _ _ _ -> return ())
    e <- m
    case e of
        Left a -> print "hoolahoola"
        Right b -> print "moolaMoola"