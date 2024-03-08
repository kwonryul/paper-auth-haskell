module User.Repository(
    UserRepositoryI(
        newUser
      , findByPaperId
      , findByPhoneNumber
      )
) where

import User.Entity
import Verification.Util
import DB
import PaperMonad
import Import

import Database.Persist.Sql

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Time
import Data.ByteString
import GHC.Stack

class PaperMonadI p => UserRepositoryI p where
    newUser :: (HasCallStack, MonadUnliftIO m) => AuthenticationType -> String -> ByteString -> String -> Maybe PhoneNumber -> UTCTime -> PaperAuthConn -> PaperMonad p m UserId
    newUser = newUserImpl
    findByPaperId :: (HasCallStack, MonadUnliftIO m) => String -> PaperAuthConn -> PaperMonad p m (Maybe (Entity User))
    findByPaperId = findByPaperIdImpl
    findByPhoneNumber :: (HasCallStack, MonadUnliftIO m) => PhoneNumber -> PaperAuthConn -> PaperMonad p m [Entity User]
    findByPhoneNumber = findByPhoneNumberImpl


newUserImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => AuthenticationType -> String -> ByteString -> String -> Maybe PhoneNumber -> UTCTime -> PaperAuthConn -> PaperMonad p m UserId
newUserImpl authenticationType paperId password name phoneNumber' registerDate conn = do
    let phoneNumber =
            case phoneNumber' of
                Just (PhoneNumber p) -> Just p
                Nothing -> Nothing
    paperLiftUnliftIO $ runReaderT (insert $ User authenticationType paperId (Just password) name phoneNumber registerDate) conn

findByPaperIdImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => String -> PaperAuthConn -> PaperMonad p m (Maybe (Entity User))
findByPaperIdImpl paperId conn = do
    paperLiftUnliftIO $ runReaderT (getBy $ UniquePaperId paperId) conn

findByPhoneNumberImpl :: (HasCallStack, UserRepositoryI p, MonadUnliftIO m) => PhoneNumber -> PaperAuthConn -> PaperMonad p m [Entity User]
findByPhoneNumberImpl (PhoneNumber phoneNumber) conn = do
    paperLiftUnliftIO $ runReaderT (selectList [UserPhoneNumber ==. Just phoneNumber] []) conn