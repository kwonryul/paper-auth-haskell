{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib(
    Resources(
        Resources
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
  , PaperAppI(
        app
      )
) where

import JWT.Entity
import Lock.Entity
import OAuth2.Client.GRpc.Controller
import OAuth2.Client.Entity
import Role.Entity
import User.Entity
import UserRole.Entity
import Verification.Entity
import Configurator
import Context
import DB
import GlobalMonad
import NestedMonad
import PaperApp

import Database.Persist.Sql
import Database.Persist.Typed
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc

import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Concurrent
import Control.Exception
import Data.Void
import Data.Proxy
import System.Environment
import GHC.Stack

type SaveConnectionFun = Ptr Void -> Ptr Void -> IO Int
type CleanConnectionFun = Int -> IO CString
type SendTokenAndCloseFun = Int -> CString -> CString -> IO CString

foreign import ccall "wrapper" genSaveConnectionFunPtr :: SaveConnectionFun -> IO (FunPtr SaveConnectionFun)
foreign import ccall "wrapper" genCleanConnectionFunPtr :: CleanConnectionFun -> IO (FunPtr CleanConnectionFun)
foreign import ccall "wrapper" genSendTokenAndCloseFunPtr :: SendTokenAndCloseFun -> IO (FunPtr SendTokenAndCloseFun)
foreign import ccall "run_oauth2_client_socket_server_c" runOAuth2ClientSocketServerC :: Int -> FunPtr SaveConnectionFun -> FunPtr CleanConnectionFun -> FunPtr SendTokenAndCloseFun -> IO CString

data Resources = Resources {
    context :: Context
  , certPath :: FilePath
  , secretKeyPath :: FilePath
  }

class (OAuth2ClientGRpcControllerI p, ContextI p, PaperAppI p) => LibI p where
    getAllResources :: Proxy p -> IO Resources
    getAllResources = getAllResourcesImpl
    getAllResources' :: (HasCallStack, MonadUnliftIO m) => GlobalMonad p m Resources
    getAllResources' = getAllResources'Impl
    startApp :: Proxy p -> Context -> FilePath -> FilePath -> IO ()
    startApp = startAppImpl
    startApp' :: (HasCallStack, MonadUnliftIO m) => Context -> FilePath -> FilePath -> GlobalMonad p m ()
    startApp' = startApp'Impl
    migratePaperAuth :: Proxy p -> Context.Context -> PaperAuthPool -> IO ()
    migratePaperAuth = migratePaperAuthImpl
    migratePaperAuth' :: (HasCallStack, MonadUnliftIO m) => PaperAuthPool -> GlobalMonad p m ()
    migratePaperAuth' = migratePaperAuth'Impl
    migratePaperAuth'' :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> GlobalMonad p m ()
    migratePaperAuth'' = migratePaperAuth''Impl

getAllResourcesImpl :: forall p. LibI p => Proxy p -> IO Resources
getAllResourcesImpl _ = runGlobalMonadWithoutLog $ getAllResources' @p

getAllResources'Impl :: forall p m. (HasCallStack, LibI p, MonadUnliftIO m) => GlobalMonad p m Resources
getAllResources'Impl = do
    context <- getContext @p
    homeDir <- globalLiftIOUnliftIO $ getEnv "HOME"
    projectDir <- globalLiftIOUnliftIO $ readFile $ homeDir ++ "/.paper-auth/project-directory"
    let certPath = projectDir ++ "resources/tls/cert.pem"
        secretKeyPath = projectDir ++ "resources/tls/secret-key.pem"
    return $ Resources {
        context, certPath, secretKeyPath
        }

startAppImpl :: forall p. LibI p => Proxy p -> Context -> FilePath -> FilePath -> IO ()
startAppImpl _ ctx certPath secretKeyPath =
    runGlobalMonad (config ctx) $ startApp' @p ctx certPath secretKeyPath

startApp'Impl :: (HasCallStack, LibI p, MonadUnliftIO m) => Context -> FilePath -> FilePath -> GlobalMonad p m ()
startApp'Impl ctx certPath secretKeyPath = do
    profile <- Control.Monad.Reader.ask
    homeDir <- globalLiftIOUnliftIO $ getEnv "HOME"
    projectDir <- globalLiftIOUnliftIO $ readFile $ homeDir ++ "/.paper-auth/project-directory"
    httpsPort <- lookupRequiredGlobal (config ctx) "port.https"
    oauth2ClientSocketPort <- lookupRequiredGlobal (config ctx) "port.oauth2-client-socket"
    let docsFilePath = projectDir ++ "generated/docs"
        staticFilePath = projectDir ++ "resources/static"
    _ <- globalLiftIOUnliftIO $ do
            forkIO $ do
                res <-
                    bracket
                        (nestedLog profile (config ctx) $ genSaveConnectionFunPtr $ saveConnectionHs profile (config ctx) (oauth2ClientSocketConnections ctx) $ paperAuthPool ctx)
                        (\saveConnectionFunPtr -> nestedLog profile (config ctx) $ freeHaskellFunPtr saveConnectionFunPtr)
                        (\saveConnectionFunPtr -> bracket
                            (nestedLog profile (config ctx) $ genCleanConnectionFunPtr $ cleanConnectionHs profile (config ctx) (oauth2ClientSocketConnections ctx) $ paperAuthPool ctx)
                            (\cleanConnectionFunPtr -> nestedLog profile (config ctx) $ freeHaskellFunPtr cleanConnectionFunPtr)
                            (\cleanConnectionFunPtr -> bracket
                                (nestedLog profile (config ctx) $ genSendTokenAndCloseFunPtr $ sendTokenAndCloseHs profile (config ctx) $ oauth2ClientSocketConnections ctx)
                                (\sendTokenAndCloseFunPtr -> nestedLog profile (config ctx) $ freeHaskellFunPtr sendTokenAndCloseFunPtr)
                                (\sendTokenAndCloseFunPtr -> do
                                    bracket
                                        (nestedLog profile (config ctx) $ runOAuth2ClientSocketServerC oauth2ClientSocketPort saveConnectionFunPtr cleanConnectionFunPtr sendTokenAndCloseFunPtr)
                                        (nestedLog profile (config ctx) . free)
                                        (nestedLog profile (config ctx) . peekCString)
                                    )
                            )
                        )
                case res of
                    "OK" ->
                        return ()
                    err ->
                        throwIO $ userError err
    globalLiftIOUnliftIO $ runTLS
        (tlsSettings certPath secretKeyPath)
        (setPort httpsPort defaultSettings)
        (app profile ctx docsFilePath staticFilePath)

migratePaperAuthImpl :: forall p. (HasCallStack, LibI p) => Proxy p -> Context.Context -> PaperAuthPool -> IO ()
migratePaperAuthImpl _ ctx = runGlobalMonad (config ctx) . migratePaperAuth' @p

migratePaperAuth'Impl :: (HasCallStack, LibI p, MonadUnliftIO m) => PaperAuthPool -> GlobalMonad p m ()
migratePaperAuth'Impl pool = runSqlPoolOneConnectionGlobal (migratePaperAuth'') pool

migratePaperAuth''Impl :: (HasCallStack, LibI p, MonadUnliftIO m) => PaperAuthConn -> GlobalMonad p m ()
migratePaperAuth''Impl conn = do
    globalLiftUnliftIO $ runReaderT (runMigration migrateJWT) (generalizeSqlBackend conn)
    globalLiftUnliftIO $ runReaderT (runMigration migrateVerification) (generalizeSqlBackend conn)
    globalLiftUnliftIO $ runReaderT (runMigration migrateUser) (generalizeSqlBackend conn)
    globalLiftUnliftIO $ runReaderT (runMigration migrateRole) (generalizeSqlBackend conn)
    globalLiftUnliftIO $ runReaderT (runMigration migrateUserRole) (generalizeSqlBackend conn)
    globalLiftUnliftIO $ runReaderT (runMigration migrateOAuth2ClientSocket) (generalizeSqlBackend conn)
    globalLiftUnliftIO $ runReaderT (runMigration migrateLock) (generalizeSqlBackend conn)