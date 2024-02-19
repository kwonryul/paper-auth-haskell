module Main where

import Lib

import GHC.Stack

main :: HasCallStack => IO ()
main = startApp