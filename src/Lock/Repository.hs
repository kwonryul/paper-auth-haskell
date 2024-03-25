module Lock.Repository(
    LockRepositoryI(
        lock
      )
) where

import Lock.Entity
import DB
import PaperMonad

import Database.Esqueleto.Experimental

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import GHC.Stack

class PaperMonadI p => LockRepositoryI p where
    lock :: (HasCallStack, MonadUnliftIO m) => [String] -> PaperAuthConn -> PaperMonad p m ()
    lock = lockImpl

lockImpl :: (HasCallStack, LockRepositoryI p, MonadUnliftIO m) => [String] -> PaperAuthConn -> PaperMonad p m ()
lockImpl nameList conn =
    paperLiftUnliftIO $ runReaderT (do
        _ <- select $ do
            l <- from $ table @Lock
            where_ (l ^. LockName `in_` valList nameList)
            locking ForUpdate
            return l
        return ()
        ) conn