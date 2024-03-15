{-# LANGUAGE TemplateHaskell #-}

module Enum(
    AuthenticationType(
        Paper
      )
  , SocketType(
        WebSocket
      , NativeSocket
      )
) where

import Definition

import Database.Persist.TH

$(defineEnum "enum/authenticationType.enum")
$(derivePersistField "AuthenticationType")

$(defineEnum "enum/socketType.enum")
$(derivePersistField "SocketType")