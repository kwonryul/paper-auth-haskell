{-# LANGUAGE TemplateHaskell #-}

module Import(
    DB
  , AuthenticationType(
        Paper
      )
) where

import Database.Persist.TH

class DB a

data AuthenticationType = Paper
  deriving (Show, Read, Eq)
$(derivePersistField "AuthenticationType")
