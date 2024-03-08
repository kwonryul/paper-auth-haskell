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