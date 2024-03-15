{-# LANGUAGE OverloadedStrings #-}

module User.Repository(
    UserRepositoryI(
        newUser
      , findByPaperId
      , findByPhoneNumber
      , verifyIdPw
      , getPreAuthenticatedUser
      )
) where

import JWT.Model
import Role.Entity
import User.Entity
import UserRole.Entity
import Verification.Util
import CallStack
import DB
import Enum
import PaperMonad

import Servant
import Database.Persist.Sql
import Crypto.BCrypt

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Set
import Data.Time
import Data.ByteString
import Data.ByteString.Char8
import GHC.Stack

class PaperMonadI p => UserRepositoryI p where
    newUser :: (HasCallStack, MonadUnliftIO m) => AuthenticationType -> String -> ByteString -> String -> Maybe PhoneNumber -> UTCTime -> PaperAuthConn -> PaperMonad p m UserId
    newUser = newUserImpl
    findByPaperId :: (HasCallStack, MonadUnliftIO m) => String -> PaperAuthConn -> PaperMonad p m (Maybe (Entity User))
    findByPaperId = findByPaperIdImpl
    findByPhoneNumber :: (HasCallStack, MonadUnliftIO m) => PhoneNumber -> PaperAuthConn -> PaperMonad p m [Entity User]
    findByPhoneNumber = findByPhoneNumberImpl
    verifyIdPw :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> String -> String -> PaperMonad p m (Entity User)
    verifyIdPw = verifyIdPwImpl
    getPreAuthenticatedUser :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> UserId -> PaperMonad p m PreAuthenticatedUser
    getPreAuthenticatedUser = getPreAuthenticatedUserImpl


newUserImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => AuthenticationType -> String -> ByteString -> String -> Maybe PhoneNumber -> UTCTime -> PaperAuthConn -> PaperMonad p m UserId
newUserImpl authenticationType paperId password name phoneNumber' registerDate conn = do
    let phoneNumber =
            case phoneNumber' of
                Just (PhoneNumber p) -> Just p
                Nothing -> Nothing
    paperLiftUnliftIO $ runReaderT (Database.Persist.Sql.insert $ User authenticationType paperId (Just password) name phoneNumber registerDate) conn

findByPaperIdImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => String -> PaperAuthConn -> PaperMonad p m (Maybe (Entity User))
findByPaperIdImpl paperId conn = do
    paperLiftUnliftIO $ runReaderT (getBy $ UniquePaperId paperId) conn

findByPhoneNumberImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => PhoneNumber -> PaperAuthConn -> PaperMonad p m [Entity User]
findByPhoneNumberImpl (PhoneNumber phoneNumber) conn = do
    paperLiftUnliftIO $ runReaderT (selectList [UserPhoneNumber ==. Just phoneNumber] []) conn

verifyIdPwImpl :: forall p m. (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> String -> String -> PaperMonad p m (Entity User)
verifyIdPwImpl conn paperId password = do
    userEntity' <- paperLiftUnliftIO $ runReaderT (getBy $ UniquePaperId paperId) conn
    rt@(Entity _ user) <- case userEntity' of
        Just userEntity -> return userEntity
        Nothing ->
            toPaperMonad $ PaperError "user not found" (err401 { errBody = "user not found" }) (callStack' profile)
    case userPassword user of
        Just userPassword' -> 
            paperAssert
                (validatePassword userPassword' (Data.ByteString.Char8.pack password))
                (PaperError "password invalid" (err401 { errBody = "password invalid" }) (callStack' profile))
        Nothing -> toPaperMonad $ PaperError "user not having password" (err401 { errBody = "try another authentication method. user not having password" }) (callStack' profile)
    return rt
    where
        profile :: Proxy p
        profile = Proxy

getPreAuthenticatedUserImpl :: forall p m. (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> UserId -> PaperMonad p m PreAuthenticatedUser
getPreAuthenticatedUserImpl conn userId = do
    userEntity' <- paperLiftUnliftIO $ runReaderT (get userId) conn
    _ <- maybeToPaperMonad userEntity' $ PaperError "user not found" (err500 { errBody = "user not found" }) (callStack' profile)
    userRoleEntityList <- paperLiftUnliftIO $ runReaderT (selectList [UserRoleUserId ==. userId] []) conn
    let roleIdList = (\(Entity _ userRole) -> userRoleRoleId userRole) <$> userRoleEntityList
    roleEntityList <- paperLiftUnliftIO $ runReaderT (selectList [RoleId <-. roleIdList] []) conn
    let roleSet = fromList $ (\(Entity _ role) -> role) <$> roleEntityList
    return $ PreAuthenticatedUser userId roleSet
    where
        profile :: Proxy p
        profile = Proxy