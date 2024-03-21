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
    newVerification :: (HasCallStack, MonadUnliftIO m) => PhoneNumber -> String -> UTCTime -> UTCTime -> UTCTime -> PaperAuthConn -> PaperMonad p m VerificationId
    newVerification = newVerificationImpl
    findByPhoneNumber :: (HasCallStack, MonadUnliftIO m) => PhoneNumber -> PaperAuthConn -> PaperMonad p m (Maybe (Entity Verification))
    findByPhoneNumber = findByPhoneNumberImpl
    deleteByPhoneNumber :: (HasCallStack, MonadUnliftIO m) => PhoneNumber -> PaperAuthConn -> PaperMonad p m ()
    deleteByPhoneNumber = deleteByPhoneNumberImpl
    deleteById :: (HasCallStack, MonadUnliftIO m) => VerificationId -> PaperAuthConn -> PaperMonad p m ()
    deleteById = deleteByIdImpl
    increaseFailCount :: (HasCallStack, MonadUnliftIO m) => VerificationId -> PaperAuthConn -> PaperMonad p m ()
    increaseFailCount = increaseFailCountImpl


newVerificationImpl :: (HasCallStack, VerificationRepositoryI p, MonadUnliftIO m) => PhoneNumber -> String -> UTCTime -> UTCTime -> UTCTime -> PaperAuthConn -> PaperMonad p m VerificationId
newVerificationImpl (PhoneNumber phoneNumber) phoneNumberSecret iat expire deleteAt conn = do
    paperLiftUnliftIO $ runReaderT (insert $ Verification phoneNumber phoneNumberSecret iat expire deleteAt 0) conn

findByPhoneNumberImpl :: (HasCallStack, VerificationRepositoryI p, MonadUnliftIO m) => PhoneNumber -> PaperAuthConn -> PaperMonad p m (Maybe (Entity Verification))
findByPhoneNumberImpl (PhoneNumber phoneNumber) conn = do
    paperLiftUnliftIO $ runReaderT (getBy $ UniquePhoneNumber phoneNumber) conn

deleteByPhoneNumberImpl :: (HasCallStack, VerificationRepositoryI p, MonadUnliftIO m) => PhoneNumber -> PaperAuthConn -> PaperMonad p m ()
deleteByPhoneNumberImpl (PhoneNumber phoneNumber) conn = do
    paperLiftUnliftIO $ runReaderT (deleteWhere [VerificationPhoneNumber ==. phoneNumber]) conn

deleteByIdImpl :: (HasCallStack, VerificationRepositoryI p, MonadUnliftIO m) => VerificationId -> PaperAuthConn -> PaperMonad p m ()
deleteByIdImpl verificationId conn = do
    paperLiftUnliftIO $ runReaderT (delete verificationId) conn

increaseFailCountImpl :: (HasCallStack, VerificationRepositoryI p, MonadUnliftIO m) => VerificationId -> PaperAuthConn -> PaperMonad p m ()
increaseFailCountImpl verificationId conn = do
    paperLiftUnliftIO $ runReaderT (update verificationId [VerificationFailCount +=. 1]) conn