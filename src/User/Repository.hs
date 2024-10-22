{-# LANGUAGE OverloadedStrings #-}

module User.Repository(
    UserRepositoryI(
        getUserInfo
      , patchUserInfo
      , newUser
      , findByPaperId
      , findByPhoneNumber
      , verifyIdPw
      , getPreAuthenticatedUser
      , findByAuthTypeAndIdentifier
      )
) where

import JWT.Model
import Role.Entity
import User.DTO
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
import Control.Monad.Reader
import Data.Set
import Data.Time
import Data.ByteString
import Data.ByteString.Char8
import GHC.Stack

class PaperMonadI p => UserRepositoryI p where
    getUserInfo :: (HasCallStack, MonadUnliftIO m) => UserId -> PaperAuthConn -> PaperMonad p m GetUserInfoResDTO
    getUserInfo = getUserInfoImpl
    patchUserInfo :: (HasCallStack, MonadUnliftIO m) => UserId -> Maybe String -> Maybe ByteString -> Maybe String -> Maybe PhoneNumber -> PaperAuthConn -> PaperMonad p m ()
    patchUserInfo = patchUserInfoImpl
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

getUserInfoImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => UserId -> PaperAuthConn -> PaperMonad p m GetUserInfoResDTO
getUserInfoImpl userId conn = do
    profile <- Control.Monad.Reader.ask
    userEntity' <- paperLiftUnliftIO $ runReaderT (get userId) conn
    User { userPaperId, userName, userPhoneNumber, userRegisterDate } <- maybeToPaperMonad userEntity' $ PaperError "user not found" (err500 { errBody = "user not found" }) (callStack' profile)
    userRoleEntityList <- paperLiftUnliftIO $ runReaderT (selectList [UserRoleUserId ==. userId] []) conn
    let roleIdList = (\(Entity _ userRole) -> userRoleRoleId userRole) <$> userRoleEntityList
    roleEntityList <- paperLiftUnliftIO $ runReaderT (selectList [RoleId <-. roleIdList] []) conn
    let roleList = (\(Entity _ role) -> roleName role) <$> roleEntityList
    return $ GetUserInfoResDTO userPaperId roleList userName userPhoneNumber userRegisterDate

patchUserInfoImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => UserId -> Maybe String -> Maybe ByteString -> Maybe String -> Maybe PhoneNumber -> PaperAuthConn -> PaperMonad p m ()
patchUserInfoImpl userId paperId password name phoneNumber conn =
    let updateList =
            [] .:.
                (UserPaperId =.) . Just <$> paperId .:.
                (UserPassword =.) . Just <$> password .:.
                (UserName =.) . Just <$> name .:.
                (UserPhoneNumber =.) . Just . (\(PhoneNumber pn) -> pn) <$> phoneNumber
    in
    paperLiftUnliftIO $ runReaderT (update userId updateList) conn
    where
        infixl 0 .:.
        (.:.) :: [a] -> Maybe a -> [a]
        (.:.) as (Just a) = a : as
        (.:.) as Nothing = as

newUserImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => AuthenticationType -> Maybe String -> Maybe ByteString -> Maybe PhoneNumber -> Maybe String -> UTCTime -> PaperAuthConn -> PaperMonad p m UserId
newUserImpl authenticationType paperId password phoneNumber' identifier registerDate conn = do
    let phoneNumber = case phoneNumber' of
            Just (PhoneNumber phoneNumber'') -> Just phoneNumber''
            Nothing -> Nothing
    paperLiftUnliftIO $ runReaderT (Database.Persist.Sql.insert $ User authenticationType paperId password phoneNumber identifier Nothing registerDate) conn

findByPaperIdImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => String -> PaperAuthConn -> PaperMonad p m [Entity User]
findByPaperIdImpl paperId conn =
    paperLiftUnliftIO $ runReaderT (selectList [UserPaperId ==. Just paperId] []) conn

findByPhoneNumberImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => PhoneNumber -> PaperAuthConn -> PaperMonad p m [Entity User]
findByPhoneNumberImpl (PhoneNumber phoneNumber) conn =
    paperLiftUnliftIO $ runReaderT (selectList [UserPhoneNumber ==. Just phoneNumber] []) conn

verifyIdPwImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => String -> String -> PaperAuthConn -> PaperMonad p m (Entity User)
verifyIdPwImpl paperId password conn = do
    profile <- Control.Monad.Reader.ask
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

getPreAuthenticatedUserImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => UserId -> PaperAuthConn -> PaperMonad p m PreAuthenticatedUser
getPreAuthenticatedUserImpl userId conn = do
    profile <- Control.Monad.Reader.ask
    user' <- paperLiftUnliftIO $ runReaderT (get userId) conn
    _ <- maybeToPaperMonad user' $ PaperError "user not found" (err500 { errBody = "user not found" }) (callStack' profile)
    userRoleEntityList <- paperLiftUnliftIO $ runReaderT (selectList [UserRoleUserId ==. userId] []) conn
    let roleIdList = (\(Entity _ userRole) -> userRoleRoleId userRole) <$> userRoleEntityList
    roleEntityList <- paperLiftUnliftIO $ runReaderT (selectList [RoleId <-. roleIdList] []) conn
    let roleSet = fromList $ (\(Entity _ role) -> role) <$> roleEntityList
    return $ PreAuthenticatedUser userId roleSet

findByAuthTypeAndIdentifierImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => AuthenticationType -> String -> PaperAuthConn -> PaperMonad p m [Entity User]
findByAuthTypeAndIdentifierImpl authenticationType identifier conn =
    paperLiftUnliftIO $ runReaderT (selectList [UserAuthenticationType ==. authenticationType, UserIdentifier ==. Just identifier] []) conn