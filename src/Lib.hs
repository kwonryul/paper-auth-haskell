module Lib(
    Resources(
        Resources
      , staticFilePath
      , context
      , certPath
      , secretKeyPath
      )
  , LibI(
        getAllResources
      , startApp
      , migratePaperAuth
      )
  , Context(
        config
      , paperAuthPool
      , paperEncodeSigner
      , paperVerifySigner
      )
  , app
) where

import JWT.Entity
import Verification.Entity
import User.Entity
import Role.Entity
import UserRole.Entity
import PaperApp
import Context
import GlobalMonad
import DB
import Paths_paper_auth

import Database.Persist.Sql
import Database.Persist.Typed
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Control.Concurrent
import Data.Proxy
import GHC.Stack

data Resources = Resources {
    staticFilePath :: FilePath
  , context :: Context
  , certPath :: FilePath
  , secretKeyPath :: FilePath
  }

class (ContextI p, PaperAppI p) => LibI p where
    getAllResources :: Proxy p -> IO Resources
    getAllResources = getAllResourcesImpl
    getAllResources' :: (HasCallStack, MonadUnliftIO m) => GlobalMonad p m Resources
    getAllResources' = getAllResources'Impl
    startApp :: Proxy p -> FilePath -> Context -> FilePath -> FilePath -> IO ()
    startApp = startAppImpl
    startApp' :: (HasCallStack, MonadUnliftIO m) => FilePath -> Context -> FilePath -> FilePath -> GlobalMonad p m ()
    startApp' = startApp'Impl
    migratePaperAuth :: Proxy p -> PaperAuthPool -> IO ()
    migratePaperAuth = migratePaperAuthImpl
    migratePaperAuth' :: (HasCallStack, MonadUnliftIO m) => PaperAuthPool -> GlobalMonad p m ()
    migratePaperAuth' = migratePaperAuth'Impl
    migratePaperAuth'' :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> GlobalMonad p m ()
    migratePaperAuth'' = migratePaperAuth''Impl

getAllResourcesImpl :: forall p. LibI p => Proxy p -> IO Resources
getAllResourcesImpl _ = runGlobalMonad $ getAllResources' @p

getAllResources'Impl :: forall p m. (HasCallStack, LibI p, MonadUnliftIO m) => GlobalMonad p m Resources
getAllResources'Impl = do
    staticFilePath <- globalLiftIOUnliftIO $ getDataFileName "resources/static"
    context <- getContext @p
    certPath <- globalLiftIOUnliftIO $ getDataFileName "resources/tls/cert.pem"
    secretKeyPath <- globalLiftIOUnliftIO $ getDataFileName "resources/tls/secret-key.pem"
    return $ Resources {
        staticFilePath, context, certPath, secretKeyPath
        }

startAppImpl :: forall p. LibI p => Proxy p -> FilePath -> Context -> FilePath -> FilePath -> IO ()
startAppImpl _ staticFilePath context certPath secretKeyPath =
    runGlobalMonad $ startApp' @p staticFilePath context certPath secretKeyPath

startApp'Impl :: forall p m. (HasCallStack, LibI p, MonadUnliftIO m) => FilePath -> Context -> FilePath -> FilePath -> GlobalMonad p m ()
startApp'Impl staticFilePath context certPath secretKeyPath = do
    _ <- globalLiftIOUnliftIO $ forkIO $ (globalLog $ run 80 (app (Proxy :: Proxy p) context staticFilePath))
    globalLiftIOUnliftIO $ runTLS
        (tlsSettings certPath secretKeyPath)
        (setPort 443 defaultSettings)
        (app (Proxy :: Proxy p) context staticFilePath)

migratePaperAuthImpl :: forall p. LibI p => Proxy p -> PaperAuthPool -> IO ()
migratePaperAuthImpl _ = runGlobalMonad . migratePaperAuth' @p

migratePaperAuth'Impl :: (HasCallStack, LibI p, MonadUnliftIO m) => PaperAuthPool -> GlobalMonad p m ()
migratePaperAuth'Impl pool = runSqlPoolOneConnectionGlobal (migratePaperAuth'') pool

migratePaperAuth''Impl :: (HasCallStack, LibI p, MonadUnliftIO m) => PaperAuthConn -> GlobalMonad p m ()
migratePaperAuth''Impl conn = do
    globalLiftUnliftIO $ runReaderT (runMigration migrateJWT) (generalizeSqlBackend conn)
    globalLiftUnliftIO $ runReaderT (runMigration migrateVerification) (generalizeSqlBackend conn)
    globalLiftUnliftIO $ runReaderT (runMigration migrateUser) (generalizeSqlBackend conn)
    globalLiftUnliftIO $ runReaderT (runMigration migrateRole) (generalizeSqlBackend conn)
    globalLiftUnliftIO $ runReaderT (runMigration migrateUserRole) (generalizeSqlBackend conn)