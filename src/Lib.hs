module Lib(
    Resources(
        Resources
      , staticFilePath
      , context
      , certPath
      , secretKeyPath
      )
  , getAllResources
  , startApp
  , migratePaperAuth
) where

import JWT.Entity
import Verification.Entity
import User.Entity
import Role.Entity
import UserRole.Entity
import PaperApp
import Context
import GlobalError
import DB
import Paths_paper_auth

import Database.Persist.Sql
import Database.Persist.Typed
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Control.Concurrent
import GHC.Stack

data Resources = Resources {
    staticFilePath :: FilePath
  , context :: Context
  , certPath :: FilePath
  , secretKeyPath :: FilePath
  }

getAllResources :: (HasCallStack, MonadUnliftIO m) => GlobalExceptT m Resources
getAllResources = do
    staticFilePath <- globalLiftIO $ getDataFileName "resources/static"
    context <- getContext'
    certPath <- globalLiftIO $ getDataFileName "resources/tls/cert.pem"
    secretKeyPath <- globalLiftIO $ getDataFileName "resources/tls/secret-key.pem"
    return $ Resources {
        staticFilePath, context, certPath, secretKeyPath
        }

startApp :: (HasCallStack, MonadUnliftIO m) => FilePath -> Context -> FilePath -> FilePath -> GlobalExceptT m ()
startApp staticFilePath context certPath secretKeyPath = do
    _ <- globalLiftIO $ forkIO $ (globalLog $ run 80 (app context staticFilePath))
    globalLiftIO $ runTLS
        (tlsSettings certPath secretKeyPath)
        (setPort 443 defaultSettings)
        (app context staticFilePath)

migratePaperAuth :: (HasCallStack, MonadUnliftIO m) => PaperAuthPool -> GlobalExceptT m ()
migratePaperAuth pool = do
    unsafeGlobalExceptTToSafe $ runSqlPoolFor (ReaderT (\conn ->
        catchE (inner conn) $ \e -> do
            globalLift $ runReaderT transactionUndo (generalizeSqlBackend conn)
            ExceptT $ return $ Left e
            )) pool
    where
        inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> GlobalExceptT m ()
        inner = migratePaperAuth'

migratePaperAuth' :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> GlobalExceptT m ()
migratePaperAuth' conn = do
    globalLift $ runReaderT (runMigration migrateJWT) (generalizeSqlBackend conn)
    globalLift $ runReaderT (runMigration migrateVerification) (generalizeSqlBackend conn)
    globalLift $ runReaderT (runMigration migrateUser) (generalizeSqlBackend conn)
    globalLift $ runReaderT (runMigration migrateRole) (generalizeSqlBackend conn)
    globalLift $ runReaderT (runMigration migrateUserRole) (generalizeSqlBackend conn)