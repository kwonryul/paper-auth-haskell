{-# LANGUAGE OverloadedStrings #-}

module Role.Repository(
    RoleRepositoryI(
        getRoleSetByNameList
      )
) where

import Role.Entity
import DB
import PaperMonad
import CallStack

import Servant
import Database.Persist.Sql

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Set
import GHC.Stack

class PaperMonadI p => RoleRepositoryI p where
    getRoleSetByNameList :: (HasCallStack, MonadUnliftIO m) => [String] -> PaperAuthConn -> PaperMonad p m (Set Role)
    getRoleSetByNameList = getRoleSetByNameListImpl

getRoleSetByNameListImpl :: forall p m. (HasCallStack, RoleRepositoryI p, MonadUnliftIO m) => [String] -> PaperAuthConn -> PaperMonad p m (Set Role)
getRoleSetByNameListImpl roleNameList conn = do
    roleEntityList <- paperLiftUnliftIO $ runReaderT (selectList [RoleName <-. roleNameList] []) conn
    paperAssert (length roleEntityList == length roleNameList) $
        PaperError "some roleName invalid" (err500 { errBody = "some roleName invalid" }) (callStack' profile)
    return $ Data.Set.fromList $ (\(Entity _ role) -> role) <$> roleEntityList
    where
        profile :: Proxy p
        profile = Proxy