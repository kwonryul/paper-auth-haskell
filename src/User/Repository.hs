{-# LANGUAGE OverloadedStrings #-}

module User.Repository(
    UserRepositoryI(
        newUser
      , findByPaperId
      , findByPhoneNumber
      , verifyIdPw
      , getPreAuthenticatedUser
      , findByAuthTypeAndIdentifier
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
    newUser :: (HasCallStack, MonadUnliftIO m) => AuthenticationType -> Maybe String -> Maybe ByteString -> Maybe PhoneNumber -> Maybe String -> UTCTime -> PaperAuthConn -> PaperMonad p m UserId
    newUser = newUserImpl
    findByPaperId :: (HasCallStack, MonadUnliftIO m) => String -> PaperAuthConn -> PaperMonad p m [Entity User]
    findByPaperId = findByPaperIdImpl
    findByPhoneNumber :: (HasCallStack, MonadUnliftIO m) => PhoneNumber -> PaperAuthConn -> PaperMonad p m [Entity User]
    findByPhoneNumber = findByPhoneNumberImpl
    verifyIdPw :: (HasCallStack, MonadUnliftIO m) => String -> String -> PaperAuthConn -> PaperMonad p m (Entity User)
    verifyIdPw = verifyIdPwImpl
    getPreAuthenticatedUser :: (HasCallStack, MonadUnliftIO m) => UserId -> PaperAuthConn -> PaperMonad p m PreAuthenticatedUser
    getPreAuthenticatedUser = getPreAuthenticatedUserImpl
    findByAuthTypeAndIdentifier :: (HasCallStack, MonadUnliftIO m) => AuthenticationType -> String -> PaperAuthConn -> PaperMonad p m [Entity User]
    findByAuthTypeAndIdentifier = findByAuthTypeAndIdentifierImpl


newUserImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => AuthenticationType -> Maybe String -> Maybe ByteString -> Maybe PhoneNumber -> Maybe String -> UTCTime -> PaperAuthConn -> PaperMonad p m UserId
newUserImpl authenticationType paperId password phoneNumber' identifier registerDate conn = do
    let phoneNumber = case phoneNumber' of
            Just (PhoneNumber phoneNumber'') -> Just phoneNumber''
            Nothing -> Nothing
    paperLiftUnliftIO $ runReaderT (Database.Persist.Sql.insert $ User authenticationType paperId password phoneNumber identifier Nothing registerDate) conn

findByPaperIdImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => String -> PaperAuthConn -> PaperMonad p m [Entity User]
findByPaperIdImpl paperId conn = do
    paperLiftUnliftIO $ runReaderT (selectList [UserPaperId ==. Just paperId] []) conn

findByPhoneNumberImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => PhoneNumber -> PaperAuthConn -> PaperMonad p m [Entity User]
findByPhoneNumberImpl (PhoneNumber phoneNumber) conn = do
    paperLiftUnliftIO $ runReaderT (selectList [UserPhoneNumber ==. Just phoneNumber] []) conn

verifyIdPwImpl :: forall p m. (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => String -> String -> PaperAuthConn -> PaperMonad p m (Entity User)
verifyIdPwImpl paperId password conn = do
    userEntityList <- paperLiftUnliftIO $ runReaderT (selectList [UserPaperId ==. Just paperId] []) conn
    rt@(Entity _ user) <- case userEntityList of
        [userEntity] -> return userEntity
        [] ->
            toPaperMonad $ PaperError "user not found" (err401 { errBody = "user not found" }) (callStack' profile)
        _ ->
            toPaperMonad $ PaperError "paperId duplicate" (err401 { errBody = "database error" }) (callStack' profile)
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

getPreAuthenticatedUserImpl :: forall p m. (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => UserId -> PaperAuthConn -> PaperMonad p m PreAuthenticatedUser
getPreAuthenticatedUserImpl userId conn = do
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

findByAuthTypeAndIdentifierImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => AuthenticationType -> String -> PaperAuthConn -> PaperMonad p m [Entity User]
findByAuthTypeAndIdentifierImpl authenticationType identifier conn = do
    paperLiftUnliftIO $ runReaderT (selectList [UserAuthenticationType ==. authenticationType, UserIdentifier ==. Just identifier] []) conn