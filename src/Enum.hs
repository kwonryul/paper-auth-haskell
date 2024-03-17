{-# LANGUAGE TemplateHaskell #-}

module Enum where

import Definition

import Database.Persist.TH

$(defineEnum "enum/authenticationType.enum")
$(derivePersistField "AuthenticationType")

$(defineEnum "enum/socketType.enum")
$(derivePersistField "SocketType")