module Verification.Repository(
    VerificationRepositoryI(
        newVerification
      , findByPhoneNumber
      , deleteByPhoneNumber
      , deleteById
      , increaseFailCount
      )
) where

import Verification.Util
import Verification.Entity
import DB
import PaperMonad

import Database.Persist

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Time
import GHC.Stack

class PaperMonadI p => VerificationRepositoryI p where
    newVerification :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PhoneNumber -> String -> UTCTime -> UTCTime -> UTCTime -> PaperMonad p m VerificationId
    newVerification = newVerificationImpl
    findByPhoneNumber :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PhoneNumber -> PaperMonad p m (Maybe (Entity Verification))
    findByPhoneNumber = findByPhoneNumberImpl
    deleteByPhoneNumber :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PhoneNumber -> PaperMonad p m ()
    deleteByPhoneNumber = deleteByPhoneNumberImpl
    deleteById :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> VerificationId -> PaperMonad p m ()
    deleteById = deleteByIdImpl
    increaseFailCount :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> VerificationId -> PaperMonad p m ()
    increaseFailCount = increaseFailCountImpl


newVerificationImpl :: (HasCallStack, VerificationRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> PhoneNumber -> String -> UTCTime -> UTCTime -> UTCTime -> PaperMonad p m VerificationId
newVerificationImpl conn (PhoneNumber phoneNumber) phoneNumberSecret iat expire deleteAt = do
    paperLiftUnliftIO $ runReaderT (insert $ Verification phoneNumber phoneNumberSecret iat expire deleteAt 0) conn

findByPhoneNumberImpl :: (HasCallStack, VerificationRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> PhoneNumber -> PaperMonad p m (Maybe (Entity Verification))
findByPhoneNumberImpl conn (PhoneNumber phoneNumber) = do
    paperLiftUnliftIO $ runReaderT (getBy $ UniquePhoneNumber phoneNumber) conn

deleteByPhoneNumberImpl :: (HasCallStack, VerificationRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> PhoneNumber -> PaperMonad p m ()
deleteByPhoneNumberImpl conn (PhoneNumber phoneNumber) = do
    paperLiftUnliftIO $ runReaderT (deleteWhere [VerificationPhoneNumber ==. phoneNumber]) conn

deleteByIdImpl :: (HasCallStack, VerificationRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> VerificationId -> PaperMonad p m ()
deleteByIdImpl conn verificationId = do
    paperLiftUnliftIO $ runReaderT (delete verificationId) conn

increaseFailCountImpl :: (HasCallStack, VerificationRepositoryI p, MonadUnliftIO m) => PaperAuthConn -> VerificationId -> PaperMonad p m ()
increaseFailCountImpl conn verificationId = do
    paperLiftUnliftIO $ runReaderT (update verificationId [VerificationFailCount +=. 1]) conn