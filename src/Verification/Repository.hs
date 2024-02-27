module Verification.Repository(
    newVerification
  , findByPhoneNumber
  , deleteByPhoneNumber
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



import Database.Persist.Typed
import Database.Persist.Sql
import User.Entity
import JWT.Entity
import UserRole.Entity
import Role.Entity

newVerification :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PhoneNumber -> String -> UTCTime -> UTCTime -> UTCTime -> PaperExceptT m VerificationId
newVerification conn (PhoneNumber phoneNumber) phoneNumberSecret iat expire deleteAt = do
    paperLift $ runReaderT (insert $ Verification phoneNumber phoneNumberSecret iat expire deleteAt) conn

findByPhoneNumber :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PhoneNumber -> PaperExceptT m [Entity Verification]
findByPhoneNumber conn (PhoneNumber phoneNumber) = do
    paperLift $ runReaderT (selectList [VerificationPhoneNumber ==. phoneNumber] []) conn

deleteByPhoneNumber :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PhoneNumber -> PaperExceptT m ()
deleteByPhoneNumber conn (PhoneNumber phoneNumber) = do
    paperLift $ runReaderT (do
        runMigration migrateUser
        runMigration migrateVerification
        runMigration migrateRole
        runMigration migrateJWT
        runMigration migrateUserRole
        ) (generalizeSqlBackend conn)
    paperLift $ runReaderT (deleteWhere [VerificationPhoneNumber ==. phoneNumber]) conn