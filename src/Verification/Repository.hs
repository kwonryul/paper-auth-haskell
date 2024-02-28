module Verification.Repository(
    newVerification
  , findByPhoneNumber
  , deleteByPhoneNumber
  , deleteById
  , increaseFailCount
) where

import Verification.Util
import Verification.Entity
import DB
import PaperError

import Database.Persist

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Time
import GHC.Stack

newVerification :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PhoneNumber -> String -> UTCTime -> UTCTime -> UTCTime -> PaperExceptT m VerificationId
newVerification conn (PhoneNumber phoneNumber) phoneNumberSecret iat expire deleteAt = do
    paperLift $ runReaderT (insert $ Verification phoneNumber phoneNumberSecret iat expire deleteAt 0) conn

findByPhoneNumber :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PhoneNumber -> PaperExceptT m (Maybe (Entity Verification))
findByPhoneNumber conn (PhoneNumber phoneNumber) = do
    paperLift $ runReaderT (getBy $ UniquePhoneNumber phoneNumber) conn

deleteByPhoneNumber :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PhoneNumber -> PaperExceptT m ()
deleteByPhoneNumber conn (PhoneNumber phoneNumber) = do
    paperLift $ runReaderT (deleteWhere [VerificationPhoneNumber ==. phoneNumber]) conn

deleteById :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> VerificationId -> PaperExceptT m ()
deleteById conn verificationId = do
    paperLift $ runReaderT (delete verificationId) conn

increaseFailCount :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> VerificationId -> PaperExceptT m ()
increaseFailCount conn verificationId = do
    paperLift $ runReaderT (update verificationId [VerificationFailCount +=. 1]) conn