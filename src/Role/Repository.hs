{-# LANGUAGE OverloadedStrings #-}

module Role.Repository(
    getRoleSetByNameList
) where

import Role.Entity
import DB
import PaperError
import CallStack

import Servant
import Database.Persist.Sql

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Set
import GHC.Stack

getRoleSetByNameList :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> [String] -> PaperExceptT m (Set Role)
getRoleSetByNameList conn roleNameList = do
    roleEntityList <- paperLift $ runReaderT (selectList [RoleName <-. roleNameList] []) conn
    paperAssert (length roleEntityList == length roleNameList) $
        PaperException "some roleName invalid" (err500 { errBody = "some roleName invalid" }) callStack'
    return $ Data.Set.fromList $ (\(Entity _ role) -> role) <$> roleEntityList