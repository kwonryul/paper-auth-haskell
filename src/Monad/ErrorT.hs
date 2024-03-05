{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Monad.ErrorT(
    InnerError
  , OuterError
  , DefaultError
  , ErrorTError(
        toInnerError
      , toOuterError
      , toSafeErrorT
      )
  , ErrorTProfile(
        defaultError
      , defaultLogger
      , defaultErrorLog
      )
  , ErrorT(
        ErrorT
      , unErrorT
    )
  , SafeErrorT(
        unSafeErrorT
      )
  , ErrorTI(
        unsafeToSafe
      , unsafeToSafeUnliftIO
      , liftSafe
      , liftSafeUnliftIO
      , liftIOSafe
      , liftIOSafeUnliftIO
      , errorLog
      , runErrorEither
      , runErrorT
      , runErrorTWithoutLog
      , maybeToErrorT
      , maybeTToErrorT
      , maybeTToErrorTUnliftIO
      , errorAssert
    )
) where

import Monad.ProfileT
import Import
import CallStack

import Control.Monad.Logger
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Catch
import Control.Monad.Error.Class
import UnliftIO.Exception
import Data.Proxy
import Data.Kind
import Data.Time
import System.IO
import GHC.Stack

type family InnerError e
type family OuterError e
type family DefaultError p

class (Show e, Exception (InnerError e), Exception (OuterError e)) => ErrorTError e where
    toOuterError :: Proxy e -> InnerError e -> OuterError e
    toInnerError :: e -> InnerError e
    toSafeErrorT :: (ErrorTProfile profile p, InnerError e ~ InnerError (DefaultError p), OuterError e ~ OuterError (DefaultError p), Monad m) => Proxy e -> InnerError e -> SafeErrorT profile p m a
    toSafeErrorT _ ie = SafeErrorT $ ErrorT $ LoggingT $ const $ ExceptT $ return $ Left $ ie

class (Profile profile, ErrorTError (DefaultError p)) => ErrorTProfile profile p where
    defaultError :: Exception e => Proxy profile -> Proxy p -> e -> CallStack -> DefaultError p
    defaultLogger :: Proxy profile -> Proxy p -> Context -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    defaultErrorLog :: Proxy profile -> Proxy p -> InnerError (DefaultError p) -> UTCTime -> (Loc, LogSource, LogLevel, LogStr)

type ErrorT :: Type -> Type -> (Type -> Type) -> Type -> Type
data ErrorT profile p m a where
    ErrorT :: (Profile profile, ErrorTProfile profile p) => {
        unErrorT :: LoggingT (ExceptT (InnerError (DefaultError p)) m) a
    } -> ErrorT profile p m a

instance (ErrorTProfile profile p, Functor m) => Functor (ErrorT profile p m) where
    fmap f (ErrorT x) = ErrorT $ fmap f x

instance (ErrorTProfile profile p, Monad m) => Applicative (ErrorT profile p m) where
    pure x = ErrorT $ pure x
    ErrorT f <*> ErrorT x = ErrorT $ f <*> x

instance (ErrorTProfile profile p, Monad m) => Monad (ErrorT profile p m) where
    return = pure
    ErrorT x >>= f = ErrorT $ x >>= unErrorT . f

instance ErrorTProfile profile p => MonadTrans (ErrorT profile p) where
    lift = ErrorT . lift . lift

instance (ErrorTProfile profile p, MonadIO m) => MonadIO (ErrorT profile p m) where
    liftIO = ErrorT . liftIO

instance (ErrorTProfile profile p, MonadThrow m) => MonadThrow (ErrorT profile p m) where
    throwM e = lift $ throwM e

instance (ErrorTProfile profile p, MonadCatch m) => MonadCatch (ErrorT profile p m) where
    catch (ErrorT (LoggingT x)) f = do
        ErrorT $ LoggingT $ (\logger ->
            Control.Monad.Catch.catch (x logger) (\e -> (runLoggingT $ unErrorT $ f e) logger)
            )

instance (ErrorTProfile profile p, MonadIO m) => MonadLogger (ErrorT profile p m) where
    monadLoggerLog loc logSource logLevel msg = ErrorT $ monadLoggerLog loc logSource logLevel msg

type SafeErrorT :: Type -> Type -> (Type -> Type) -> Type -> Type
data SafeErrorT profile p m a where
    SafeErrorT :: (Profile profile, ErrorTProfile profile p) => {
        unSafeErrorT :: ErrorT profile p m a
    } -> SafeErrorT profile p m a

instance (ErrorTProfile profile p, Functor m) => Functor (SafeErrorT profile p m) where
    fmap f (SafeErrorT x) = SafeErrorT $ fmap f x

instance (ErrorTProfile profile p, Monad m) => Applicative (SafeErrorT profile p m) where
    pure x = SafeErrorT $ pure x
    SafeErrorT f <*> SafeErrorT x = SafeErrorT $ f <*> x

instance (ErrorTProfile profile p, Monad m) => Monad (SafeErrorT profile p m) where
    return = pure
    SafeErrorT x >>= f = SafeErrorT $ x >>= unSafeErrorT . f

instance (ErrorTProfile profile p, MonadIO m) => MonadLogger (SafeErrorT profile p m) where
    monadLoggerLog loc logSource logLevel msg = SafeErrorT $ monadLoggerLog loc logSource logLevel msg

class CallStackI profile => ErrorTI profile where
    unsafeToSafe :: forall p m a. (HasCallStack, ErrorTProfile profile p, MonadCatch m) => ErrorT profile p m a -> SafeErrorT profile p m a
    unsafeToSafe = unsafeToSafeImpl
    unsafeToSafeUnliftIO :: forall p m a. (HasCallStack, ErrorTProfile profile p, MonadUnliftIO m) => ErrorT profile p m a -> SafeErrorT profile p m a
    unsafeToSafeUnliftIO = unsafeToSafeUnliftIOImpl
    liftSafe :: (HasCallStack, ErrorTProfile profile p, MonadCatch m) => m a -> SafeErrorT profile p m a
    liftSafe = liftSafeImpl
    liftSafeUnliftIO :: (HasCallStack, ErrorTProfile profile p, MonadUnliftIO m) => m a -> SafeErrorT profile p m a
    liftSafeUnliftIO = liftSafeUnliftIOImpl
    liftIOSafe :: (HasCallStack, ErrorTProfile profile p, MonadIO m, MonadCatch m) => IO a -> SafeErrorT profile p m a
    liftIOSafe = liftIOSafeImpl
    liftIOSafeUnliftIO :: (HasCallStack, ErrorTProfile profile p, MonadUnliftIO m) => IO a -> SafeErrorT profile p m a
    liftIOSafeUnliftIO = liftIOSafeUnliftIOImpl
    errorLog :: forall p m a. (HasCallStack, ErrorTProfile profile p, MonadError (OuterError (DefaultError p)) m, MonadIO m, MonadCatch m) => Proxy profile -> Proxy p -> Context -> IO a -> m a
    errorLog = errorLogImpl
    runErrorEither :: forall p m a. (HasCallStack, ErrorTProfile profile p, MonadError (OuterError (DefaultError p)) m, MonadIO m, MonadCatch m) => Proxy profile -> Proxy p -> Context -> Either (InnerError (DefaultError p)) a -> m a
    runErrorEither = runErrorEitherImpl
    runErrorEitherWithoutLog :: forall p m a. (HasCallStack, ErrorTProfile profile p, MonadError (OuterError (DefaultError p)) m, MonadIO m, MonadCatch m) => Proxy profile -> Proxy p -> Either (InnerError (DefaultError p)) a -> m a
    runErrorEitherWithoutLog = runErrorEitherWithoutLogImpl
    runErrorT :: forall p m a. (HasCallStack, ErrorTProfile profile p, MonadError (OuterError (DefaultError p)) m, MonadIO m, MonadCatch m) => Context -> SafeErrorT profile p IO a -> m a
    runErrorT = runErrorTImpl
    runErrorTWithoutLog :: forall p m a. (HasCallStack, ErrorTProfile profile p, MonadError (OuterError (DefaultError p)) m, MonadIO m, MonadCatch m) => SafeErrorT profile p IO a -> m a
    runErrorTWithoutLog = runErrorTWithoutLogImpl
    maybeToErrorT :: forall e p m a. (ErrorTError e, ErrorTProfile profile p, InnerError e ~ InnerError (DefaultError p), OuterError e ~ OuterError (DefaultError p), Monad m) => Maybe a -> e -> SafeErrorT profile p m a
    maybeToErrorT = maybeToErrorTImpl
    maybeTToErrorT :: forall e p m a. (HasCallStack, ErrorTError e, ErrorTProfile profile p, InnerError e ~ InnerError (DefaultError p), OuterError e ~ OuterError (DefaultError p), MonadIO m, MonadCatch m) => MaybeT IO a -> e -> SafeErrorT profile p m a
    maybeTToErrorT = maybeTToErrorTImpl
    maybeTToErrorTUnliftIO :: forall e p m a. (HasCallStack, ErrorTError e, ErrorTProfile profile p, InnerError e ~ InnerError (DefaultError p), OuterError e ~ OuterError (DefaultError p), MonadUnliftIO m) => MaybeT IO a -> e -> SafeErrorT profile p m a
    maybeTToErrorTUnliftIO = maybeTToErrorTUnliftIOImpl
    errorAssert :: forall e p m. (ErrorTError e, ErrorTProfile profile p, InnerError e ~ InnerError (DefaultError p), OuterError e ~ OuterError (DefaultError p), Monad m) => Bool -> e -> SafeErrorT profile p m ()
    errorAssert = errorAssertImpl

unsafeToSafeImpl :: forall profile p m a. (HasCallStack, ErrorTI profile, ErrorTProfile profile p, MonadCatch m) => ErrorT profile p m a -> SafeErrorT profile p m a
unsafeToSafeImpl x = SafeErrorT $ Control.Monad.Catch.catch x (\(e :: SomeException) ->
    ErrorT $ LoggingT $ const $ ExceptT $ return $ Left $ toInnerError $ defaultError profile p e $ callStack' profile
    )
    where
        profile :: Proxy profile
        profile = Proxy
        p :: Proxy p
        p = Proxy

unsafeToSafeUnliftIOImpl :: forall profile p m a. (HasCallStack, ErrorTI profile, ErrorTProfile profile p, MonadUnliftIO m) => ErrorT profile p m a -> SafeErrorT profile p m a
unsafeToSafeUnliftIOImpl (ErrorT (LoggingT x)) = SafeErrorT $ ErrorT $ LoggingT $ \logger -> do
    let ExceptT mpa = x logger
    pa <- ExceptT $ UnliftIO.Exception.catch (Right <$> mpa) (\(ex :: SomeException) ->
        return $ Left $ toInnerError $ defaultError profile p ex $ callStack' profile)
    case pa of
        Right a -> return a
        Left ex -> ExceptT $ return $ Left ex
    where
        profile :: Proxy profile
        profile = Proxy
        p :: Proxy p
        p = Proxy


liftSafeImpl :: (HasCallStack, ErrorTI profile, ErrorTProfile profile p, MonadCatch m) => m a -> SafeErrorT profile p m a
liftSafeImpl = unsafeToSafe . lift

liftSafeUnliftIOImpl :: (HasCallStack, ErrorTI profile, ErrorTProfile profile p, MonadUnliftIO m) => m a -> SafeErrorT profile p m a
liftSafeUnliftIOImpl = unsafeToSafeUnliftIO . lift

liftIOSafeImpl :: (HasCallStack, ErrorTI profile, ErrorTProfile profile p, MonadIO m, MonadCatch m) => IO a -> SafeErrorT profile p m a
liftIOSafeImpl = unsafeToSafe . liftIO

liftIOSafeUnliftIOImpl :: (HasCallStack, ErrorTI profile, ErrorTProfile profile p, MonadUnliftIO m) => IO a -> SafeErrorT profile p m a
liftIOSafeUnliftIOImpl = unsafeToSafeUnliftIO . liftIO

errorLogImpl :: forall profile p m a. (HasCallStack, ErrorTI profile, ErrorTProfile profile p, MonadError (OuterError (DefaultError p)) m, MonadIO m, MonadCatch m) => Proxy profile -> Proxy p -> Context -> IO a -> m a
errorLogImpl profile p context io = Control.Monad.Catch.catch (liftIO io) (\(ex :: SomeException) -> do
    let de = defaultError profile p ex $ callStack' profile
    let ie = toInnerError de
    currentTime <- liftIO getCurrentTime
    let (loc, logSource, logLevel, logStr) = defaultErrorLog profile p ie currentTime
    liftIO $ defaultLogger profile p context loc logSource logLevel logStr
    throwError $ toOuterError (Proxy :: Proxy (DefaultError p)) ie
    )

runErrorEitherImpl :: forall profile p m a. (HasCallStack, ErrorTI profile, ErrorTProfile profile p, MonadError (OuterError (DefaultError p)) m, MonadIO m, MonadCatch m) => Proxy profile -> Proxy p -> Context -> Either (InnerError (DefaultError p)) a -> m a
runErrorEitherImpl profile p context e = case e of
    Left ex -> do
        currentTime <- liftIO getCurrentTime
        let (loc, logSource, logLevel, logStr) = defaultErrorLog profile p ex currentTime
        liftIO $ defaultLogger profile p context loc logSource logLevel logStr
        throwError $ toOuterError (Proxy :: Proxy (DefaultError p)) ex
    Right x -> return x

runErrorEitherWithoutLogImpl :: forall profile p m a. (HasCallStack, ErrorTI profile, ErrorTProfile profile p, MonadError (OuterError (DefaultError p)) m, MonadIO m, MonadCatch m) => Proxy profile -> Proxy p -> Either (InnerError (DefaultError p)) a -> m a
runErrorEitherWithoutLogImpl profile p e = case e of
    Left ex -> do
        currentTime <- liftIO getCurrentTime
        let (loc, logSource, logLevel, logStr) = defaultErrorLog profile p ex currentTime
        liftIO $ defaultLoggerWithoutLog loc logSource logLevel logStr
        throwError $ toOuterError (Proxy :: Proxy (DefaultError p)) ex
    Right x -> return x
    where
        defaultLoggerWithoutLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
        defaultLoggerWithoutLog = defaultOutput stdout

runErrorTImpl :: forall profile p m a. (HasCallStack, ErrorTI profile, ErrorTProfile profile p, MonadError (OuterError (DefaultError p)) m, MonadIO m, MonadCatch m) => Context -> SafeErrorT profile p IO a -> m a
runErrorTImpl context (SafeErrorT (ErrorT (LoggingT x))) = do
    e <- liftIO $ runExceptT $ (x $ defaultLogger profile p context)
    runErrorEither profile p context e
    where
        profile :: Proxy profile
        profile = Proxy
        p :: Proxy p
        p = Proxy

runErrorTWithoutLogImpl :: forall profile p m a. (HasCallStack, ErrorTI profile, ErrorTProfile profile p, MonadError (OuterError (DefaultError p)) m, MonadIO m, MonadCatch m) => SafeErrorT profile p IO a -> m a
runErrorTWithoutLogImpl (SafeErrorT (ErrorT (LoggingT x))) = do
    e <- liftIO $ runExceptT $ (x $ defaultLoggerWithoutLog)
    runErrorEitherWithoutLog profile p e
    where
        profile :: Proxy profile
        profile = Proxy
        p :: Proxy p
        p = Proxy
        defaultLoggerWithoutLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
        defaultLoggerWithoutLog = defaultOutput stdout

maybeToErrorTImpl :: forall e profile p m a. (ErrorTError e, ErrorTI profile, ErrorTProfile profile p, InnerError e ~ InnerError (DefaultError p), OuterError e ~ OuterError (DefaultError p), Monad m) => Maybe a -> e -> SafeErrorT profile p m a
maybeToErrorTImpl a' ex =
    case a' of
        Just a -> return a
        Nothing -> toSafeErrorT (Proxy :: Proxy e) $ toInnerError ex

maybeTToErrorTImpl :: forall e profile p m a. (HasCallStack, ErrorTError e, ErrorTI profile, ErrorTProfile profile p, InnerError e ~ InnerError (DefaultError p), OuterError e ~ OuterError (DefaultError p), MonadIO m, MonadCatch m) => MaybeT IO a -> e -> SafeErrorT profile p m a
maybeTToErrorTImpl (MaybeT ima) ex = do
    a' <- liftIOSafe ima
    case a' of
        Just a -> return a
        Nothing -> toSafeErrorT (Proxy :: Proxy e) $ toInnerError ex

maybeTToErrorTUnliftIOImpl :: forall e profile p m a. (HasCallStack, ErrorTError e, ErrorTI profile, ErrorTProfile profile p, InnerError e ~ InnerError (DefaultError p), OuterError e ~ OuterError (DefaultError p), MonadUnliftIO m) => MaybeT IO a -> e -> SafeErrorT profile p m a
maybeTToErrorTUnliftIOImpl (MaybeT ima) ex = do
    a' <- liftIOSafeUnliftIO ima
    case a' of
        Just a -> return a
        Nothing -> toSafeErrorT (Proxy :: Proxy e) $ toInnerError ex

errorAssertImpl :: forall e profile p m. (ErrorTError e, ErrorTI profile, ErrorTProfile profile p, InnerError e ~ InnerError (DefaultError p), OuterError e ~ OuterError (DefaultError p), Monad m) => Bool -> e -> SafeErrorT profile p m ()
errorAssertImpl b ex =
    if b then
        return ()
    else
        toSafeErrorT (Proxy :: Proxy e) $ toInnerError ex