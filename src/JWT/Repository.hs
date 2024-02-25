{-# LANGUAGE OverloadedStrings #-}

module JWT.Repository(
    getAuthenticatedUser
) where

import JWT.Model
import User.Entity
import Role.Entity
import UserRole.Entity
import DB
import PaperError
import CallStack

import Servant
import Database.Persist.Sql

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader
import Data.Set
import GHC.Stack

getAuthenticatedUser :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> String -> String -> PaperExceptT m AuthenticatedUser
getAuthenticatedUser conn paperId password = do
    userEntityList <- paperLiftIO $ runReaderT (selectList [UserPaperId ==. paperId] []) conn
    (userId, user) <- case userEntityList of
        (Entity userId user : []) ->
            return (userId, user)
        (_ : _ : _) ->
            toPaperExceptT $ PaperException "paperId duplicate" (err500 { errBody = "Internal server error" }) callStack'
        [] ->
            toPaperExceptT $ PaperException "user not found" (err401 { errBody = "user not found" }) callStack'
    if userPassword user == password then
        return ()
    else
        toPaperExceptT $ PaperException "password invalid" (err401 { errBody = "password invalid" }) callStack'
    userRoleEntityList <- paperLiftIO $ runReaderT (selectList [UserRoleUserId ==. userId] []) conn
    let roleIdList = (\(Entity _ userRole) -> userRoleRoleId userRole) <$> userRoleEntityList
    roleEntityList <- paperLiftIO $ runReaderT (selectList [RoleId <-. roleIdList] []) conn
    let roleSet = fromList $ (\(Entity _ role) -> role) <$> roleEntityList
    return $ AuthenticatedUser userId roleSet