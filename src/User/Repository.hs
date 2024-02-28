module User.Repository(
    newUser
  , findByPaperId
  , findByPhoneNumber
) where

import User.Entity
import Verification.Util
import DB
import PaperError
import Import

import Database.Persist.Sql

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Time
import Data.ByteString
import GHC.Stack

newUser :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> AuthenticationType -> String -> ByteString -> String -> Maybe PhoneNumber -> UTCTime -> PaperExceptT m UserId
newUser conn authenticationType paperId password name phoneNumber' registerDate = do
    let phoneNumber =
            case phoneNumber' of
                Just (PhoneNumber p) -> Just p
                Nothing -> Nothing
    paperLift $ runReaderT (insert $ User authenticationType paperId password name phoneNumber registerDate) conn

findByPaperId :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> String -> PaperExceptT m (Maybe (Entity User))
findByPaperId conn paperId = do
    paperLift $ runReaderT (getBy $ UniquePaperId paperId) conn

findByPhoneNumber :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PhoneNumber -> PaperExceptT m [Entity User]
findByPhoneNumber conn (PhoneNumber phoneNumber) = do
    paperLift $ runReaderT (selectList [UserPhoneNumber ==. Just phoneNumber] []) conn