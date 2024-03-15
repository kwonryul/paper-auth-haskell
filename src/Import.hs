{-# LANGUAGE TemplateHaskell #-}

module Import(
    Context(
        Context
      , config
      , paperAuthPool
      , paperEncodeSigner
      , paperVerifySigner
      , oauth2ClientSocketConnections
      )
  , DB
  , PaperAuthDB(PaperAuthDB)
  , PaperAuthConn
  , PaperAuthPool
  , OAuth2ClientSocketId'
  , OAuth2ClientSocketConnections
) where

import OAuth2.Client.Model

import Database.Persist.Typed
import Data.Configurator.Types
import Web.JWT

import Control.Concurrent.MVar
import Data.Map

type OAuth2ClientSocketId' = Int
type OAuth2ClientSocketConnections = MVar (Map OAuth2ClientSocketId' OAuth2ClientSocketConnection)

data Context = Context {
    config :: Config
  , paperAuthPool :: PaperAuthPool
  , paperEncodeSigner :: EncodeSigner
  , paperVerifySigner :: VerifySigner
  , oauth2ClientSocketConnections :: OAuth2ClientSocketConnections
}

class DB a

data PaperAuthDB = PaperAuthDB deriving Show
instance DB PaperAuthDB
type PaperAuthConn = SqlFor PaperAuthDB
type PaperAuthPool = ConnectionPoolFor PaperAuthDB