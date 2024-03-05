{-# LANGUAGE TemplateHaskell #-}

module Import(
    Context(
        Context
      , config
      , paperAuthPool
      , paperEncodeSigner
      , paperVerifySigner
      )
  , DB
  , PaperAuthDB(PaperAuthDB)
  , PaperAuthConn
  , PaperAuthPool
  , AuthenticationType(
        Paper
      )
) where


import Database.Persist.Typed
import Database.Persist.TH
import Data.Configurator.Types
import Web.JWT

data Context = Context {
    config :: Config
  , paperAuthPool :: PaperAuthPool
  , paperEncodeSigner :: EncodeSigner
  , paperVerifySigner :: VerifySigner
}

class DB a

data PaperAuthDB = PaperAuthDB deriving Show
instance DB PaperAuthDB
type PaperAuthConn = SqlFor PaperAuthDB
type PaperAuthPool = ConnectionPoolFor PaperAuthDB

data AuthenticationType = Paper
  deriving (Show, Read, Eq)
$(derivePersistField "AuthenticationType")
