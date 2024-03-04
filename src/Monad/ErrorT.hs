{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Monad.ErrorT(
    ErrorTError(
        InnerError
      , OuterError
      , toInnerError
      , toOuterError
      , toSafeErrorT
      )
  , ErrorTProfile(
        DefaultError
      , defaultError
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
  , unsafeToSafe
  , unsafeToSafeUnliftIO
  , liftSafe
  , liftSafeUnliftIO
  , liftIOSafe
  , liftIOSafeUnliftIO
  , errorLog
  , runErrorEither
  , runErrorT
  , maybeToErrorT
  , maybeTToErrorT
  , maybeTToErrorTUnliftIO
  , errorAssert
  , catchEither
) where

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
import GHC.Stack

class (Show e, Exception (InnerError e), Exception (OuterError e)) => ErrorTError e where
    type InnerError e :: Type
    type OuterError e :: Type
    toOuterError :: Proxy e -> InnerError e -> OuterError e
    toInnerError :: e -> InnerError e
    toSafeErrorT :: (ErrorTProfile p, InnerError e ~ InnerError (DefaultError p), OuterError e ~ OuterError (DefaultError p), Monad m) => Proxy e -> InnerError e -> SafeErrorT p m a
    toSafeErrorT _ ie = SafeErrorT $ ErrorT $ LoggingT $ const $ ExceptT $ return $ Left $ ie
class ErrorTError (DefaultError p) => ErrorTProfile p where
    type DefaultError p = de | de -> p
    defaultError :: Exception e => e -> CallStack -> DefaultError p
    defaultLogger :: Proxy p -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    defaultErrorLog :: Proxy p -> InnerError (DefaultError p) -> UTCTime -> (Loc, LogSource, LogLevel, LogStr)

type ErrorT :: Type -> (Type -> Type) -> Type -> Type
data ErrorT p m a where
    ErrorT :: (ErrorTProfile p) => {
        unErrorT :: LoggingT (ExceptT (InnerError (DefaultError p)) m) a
    } -> ErrorT p m a

instance (ErrorTProfile p, Functor m) => Functor (ErrorT p m) where
    fmap f (ErrorT x) = ErrorT $ fmap f x

instance (ErrorTProfile p, Monad m) => Applicative (ErrorT p m) where
    pure x = ErrorT $ pure x
    ErrorT f <*> ErrorT x = ErrorT $ f <*> x

instance (ErrorTProfile p, Monad m) => Monad (ErrorT p m) where
    return = pure
    ErrorT x >>= f = ErrorT $ x >>= unErrorT . f

instance ErrorTProfile p => MonadTrans (ErrorT p) where
    lift = ErrorT . lift . lift

instance (ErrorTProfile p, MonadIO m) => MonadIO (ErrorT p m) where
    liftIO = ErrorT . liftIO

instance (ErrorTProfile p, MonadThrow m) => MonadThrow (ErrorT p m) where
    throwM e = lift $ throwM e

instance (ErrorTProfile p, MonadCatch m) => MonadCatch (ErrorT p m) where
    catch (ErrorT (LoggingT x)) f = do
        ErrorT $ LoggingT $ (\logger ->
            Control.Monad.Catch.catch (x logger) (\e -> (runLoggingT $ unErrorT $ f e) logger)
            )

instance (ErrorTProfile p, MonadIO m) => MonadLogger (ErrorT p m) where
    monadLoggerLog loc logSource logLevel msg = ErrorT $ monadLoggerLog loc logSource logLevel msg

type SafeErrorT :: Type -> (Type -> Type) -> Type -> Type
data SafeErrorT p m a where
    SafeErrorT :: (ErrorTProfile p) => {
        unSafeErrorT :: ErrorT p m a
    } -> SafeErrorT p m a

instance (ErrorTProfile p, Functor m) => Functor (SafeErrorT p m) where
    fmap f (SafeErrorT x) = SafeErrorT $ fmap f x

instance (ErrorTProfile p, Monad m) => Applicative (SafeErrorT p m) where
    pure x = SafeErrorT $ pure x
    SafeErrorT f <*> SafeErrorT x = SafeErrorT $ f <*> x

instance (ErrorTProfile p, Monad m) => Monad (SafeErrorT p m) where
    return = pure
    SafeErrorT x >>= f = SafeErrorT $ x >>= unSafeErrorT . f

instance (ErrorTProfile p, MonadIO m) => MonadLogger (SafeErrorT p m) where
    monadLoggerLog loc logSource logLevel msg = SafeErrorT $ monadLoggerLog loc logSource logLevel msg

unsafeToSafe :: forall p m a. (HasCallStack, ErrorTProfile p, MonadCatch m) => ErrorT p m a -> SafeErrorT p m a
unsafeToSafe x = SafeErrorT $ Control.Monad.Catch.catch x (\(e :: SomeException) ->
    ErrorT $ LoggingT $ const $ ExceptT $ return $ Left $ toInnerError $ (defaultError e callStack' :: DefaultError p)
    )

unsafeToSafeUnliftIO :: forall p m a. (HasCallStack, ErrorTProfile p, MonadUnliftIO m) => ErrorT p m a -> SafeErrorT p m a
unsafeToSafeUnliftIO (ErrorT (LoggingT x)) = SafeErrorT $ ErrorT $ LoggingT $ \logger -> do
    let ExceptT mpa = x logger
    pa <- ExceptT $ UnliftIO.Exception.catch (Right <$> mpa) (\(ex :: SomeException) ->
        return $ Left $ toInnerError $ (defaultError ex callStack' :: DefaultError p))
    case pa of
        Right a -> return a
        Left ex -> ExceptT $ return $ Left ex


liftSafe :: (HasCallStack, ErrorTProfile p, MonadCatch m) => m a -> SafeErrorT p m a
liftSafe = unsafeToSafe . lift

liftSafeUnliftIO :: (HasCallStack, ErrorTProfile p, MonadUnliftIO m) => m a -> SafeErrorT p m a
liftSafeUnliftIO = unsafeToSafeUnliftIO . lift

liftIOSafe :: (HasCallStack, ErrorTProfile p, MonadIO m, MonadCatch m) => IO a -> SafeErrorT p m a
liftIOSafe = unsafeToSafe . liftIO

liftIOSafeUnliftIO :: (HasCallStack, ErrorTProfile p, MonadUnliftIO m) => IO a -> SafeErrorT p m a
liftIOSafeUnliftIO = unsafeToSafeUnliftIO . liftIO

errorLog :: forall p m a. (HasCallStack, ErrorTProfile p, MonadError (OuterError (DefaultError p)) m, MonadIO m, MonadCatch m) => Proxy p -> IO a -> m a
errorLog p io = Control.Monad.Catch.catch (liftIO io) (\(ex :: SomeException) -> do
    let de = defaultError ex callStack' :: DefaultError p
    let ie = toInnerError de
    currentTime <- liftIO getCurrentTime
    let (loc, logSource, logLevel, logStr) = defaultErrorLog p ie currentTime
    liftIO $ defaultLogger p loc logSource logLevel logStr
    throwError $ toOuterError (Proxy :: Proxy (DefaultError p)) ie
    )

runErrorEither :: forall p m a. (HasCallStack, ErrorTProfile p, MonadError (OuterError (DefaultError p)) m, MonadIO m, MonadCatch m) => Proxy p -> Either (InnerError (DefaultError p)) a -> m a
runErrorEither p e = case e of
    Left ex -> do
        currentTime <- liftIO getCurrentTime
        let (loc, logSource, logLevel, logStr) = defaultErrorLog p ex currentTime
        liftIO $ defaultLogger p loc logSource logLevel logStr
        throwError $ toOuterError (Proxy :: Proxy (DefaultError p)) ex
    Right x -> return x

runErrorT :: forall p m a. (HasCallStack, ErrorTProfile p, MonadError (OuterError (DefaultError p)) m, MonadIO m, MonadCatch m) => SafeErrorT p IO a -> m a
runErrorT (SafeErrorT (ErrorT (LoggingT x))) = do
    e <- liftIO $ runExceptT $ (x $ defaultLogger (Proxy :: Proxy p))
    runErrorEither (Proxy :: Proxy p) e

maybeToErrorT :: forall e p m a. (ErrorTError e, ErrorTProfile p, InnerError e ~ InnerError (DefaultError p), OuterError e ~ OuterError (DefaultError p), Monad m) => Maybe a -> e -> SafeErrorT p m a
maybeToErrorT a' ex =
    case a' of
        Just a -> return a
        Nothing -> toSafeErrorT (Proxy :: Proxy e) $ toInnerError ex

maybeTToErrorT :: forall e p m a. (HasCallStack, ErrorTError e, ErrorTProfile p, InnerError e ~ InnerError (DefaultError p), OuterError e ~ OuterError (DefaultError p), MonadIO m, MonadCatch m) => MaybeT IO a -> e -> SafeErrorT p m a
maybeTToErrorT (MaybeT ima) ex = do
    a' <- liftIOSafe ima
    case a' of
        Just a -> return a
        Nothing -> toSafeErrorT (Proxy :: Proxy e) $ toInnerError ex

maybeTToErrorTUnliftIO :: forall e p m a. (HasCallStack, ErrorTError e, ErrorTProfile p, InnerError e ~ InnerError (DefaultError p), OuterError e ~ OuterError (DefaultError p), MonadUnliftIO m) => MaybeT IO a -> e -> SafeErrorT p m a
maybeTToErrorTUnliftIO (MaybeT ima) ex = do
    a' <- liftIOSafeUnliftIO ima
    case a' of
        Just a -> return a
        Nothing -> toSafeErrorT (Proxy :: Proxy e) $ toInnerError ex

errorAssert :: forall e p m. (ErrorTError e, ErrorTProfile p, InnerError e ~ InnerError (DefaultError p), OuterError e ~ OuterError (DefaultError p), Monad m) => Bool -> e -> SafeErrorT p m ()
errorAssert b ex =
    if b then
        return ()
    else
        toSafeErrorT (Proxy :: Proxy e) $ toInnerError ex

catchEither :: (HasCallStack, ErrorTProfile p, Monad m) => SafeErrorT p m a -> (Proxy (DefaultError p) -> InnerError (DefaultError p) -> SafeErrorT p m a) -> SafeErrorT p m a
catchEither (SafeErrorT (ErrorT (LoggingT f))) g = SafeErrorT $ ErrorT $ LoggingT $ (\logger ->
    catchE (f logger) $ \e ->
        runLoggingT (unErrorT $ unSafeErrorT $ g Proxy e) logger
    )