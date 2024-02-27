module User.Repository(
    newUser
  , findByPaperId
  , findByPhoneNumber
) where

import User.Entity
import JWT.Model
import Verification.Util
import DB
import PaperError

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
        authType = show authenticationType
    paperLift $ runReaderT (insert $ User authType paperId password name phoneNumber registerDate) conn

findByPaperId :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> String -> PaperExceptT m [Entity User]
findByPaperId conn paperId = do
    paperLift $ runReaderT (selectList [UserPaperId ==. paperId] []) conn

findByPhoneNumber :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PhoneNumber -> PaperExceptT m [Entity User]
findByPhoneNumber conn (PhoneNumber phoneNumber) = do
    paperLift $ runReaderT (selectList [UserPhoneNumber ==. Just phoneNumber] []) conn