{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GlobalMonad(
    GlobalError(
        GlobalError
      , globalErrorMsg
      , globalErrorCallStack
      )
  , GlobalDefaultError(
        GlobalDefaultError
      )
  , GlobalCatchError(
        GlobalCatchError
      )
  , GlobalInnerError(
        GlobalInnerError
      )
  , GlobalErrorP
  , GlobalMonad(
        unGlobalMonad
      )
  , GlobalMonadI(
        toGlobalMonad
      , safeErrorTToGlobalMonad
      , globalLift
      , globalLiftUnliftIO
      , globalLiftIO
      , globalLiftIOUnliftIO
      , globalLog
      , runGlobalErrorEither
      , runGlobalMonad
      , runGlobalMonadWithoutLog
      , maybeToGlobalMonad
      , maybeTToGlobalMonad
      , maybeTToGlobalMonadUnliftIO
      , globalAssert
      , globalCatch
      )
) where

import Monad.ErrorT
import Monad.ProfileT

import Servant
import Data.Configurator.Types

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Unlift
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Exception
import Data.Kind
import GHC.Stack

data GlobalError = GlobalError {
    globalErrorMsg :: String
  , globalErrorCallStack :: CallStack
  }
instance Show GlobalError where
    show (GlobalError msg cs) = "[GlobalError]\n" ++ msg ++ "\n\n" ++ prettyCallStack cs

data GlobalDefaultError where
    GlobalDefaultError :: Exception e => e -> CallStack -> GlobalDefaultError
instance Show GlobalDefaultError where
    show (GlobalDefaultError ex cs) = "[GlobalDefaultError]\n" ++ show ex ++ "\n" ++ prettyCallStack cs

data GlobalCatchError where
    GlobalCatchError :: Show e => e -> CallStack -> GlobalCatchError
instance Show GlobalCatchError where
    show (GlobalCatchError ex cs) = "[GlobalCatchError]\n" ++ show ex ++ "\n" ++ prettyCallStack cs

data GlobalInnerError where
    GlobalInnerError :: Show e => e -> GlobalInnerError
instance Show GlobalInnerError where
    show (GlobalInnerError e) = show e
instance Exception GlobalInnerError

data GlobalErrorP

type instance InnerError GlobalError = GlobalInnerError
type instance InnerError GlobalDefaultError = GlobalInnerError
type instance InnerError GlobalCatchError = GlobalInnerError
type instance OuterError GlobalError = IOException
type instance OuterError GlobalDefaultError = IOException
type instance OuterError GlobalCatchError = IOException
type instance DefaultError GlobalErrorP = GlobalDefaultError

instance ErrorTError GlobalError where
    toInnerError = GlobalInnerError
instance ErrorTError GlobalDefaultError where
    toInnerError = GlobalInnerError
instance ErrorTError GlobalCatchError where
    toInnerError = GlobalInnerError

type GlobalMonad :: Type -> (Type -> Type) -> Type -> Type
data GlobalMonad profile m a where
    GlobalMonad :: Profile profile => {
        unGlobalMonad :: ProfileT profile (SafeErrorT profile GlobalErrorP m) a
    } -> GlobalMonad profile m a

instance (Profile profile, ErrorTProfile profile GlobalErrorP, Functor m) => Functor (GlobalMonad profile m) where
    fmap f (GlobalMonad p) = GlobalMonad $ fmap f p

instance (Profile profile, ErrorTProfile profile GlobalErrorP, Monad m) => Applicative (GlobalMonad profile m) where
    pure x = GlobalMonad $ pure x
    GlobalMonad f <*> GlobalMonad x = GlobalMonad $ f <*> x

instance (Profile profile, ErrorTProfile profile GlobalErrorP, Monad m) => Monad (GlobalMonad profile m) where
    return = pure
    GlobalMonad x >>= f = GlobalMonad $ x >>= unGlobalMonad . f

instance (Profile profile, ErrorTProfile profile GlobalErrorP, Monad m) => MonadReader (Proxy profile) (GlobalMonad profile m) where
    ask = GlobalMonad $ ask
    local f x = GlobalMonad $ local f $ unGlobalMonad x

class (ErrorTI profile, ErrorTProfile profile GlobalErrorP) => GlobalMonadI profile where
    toGlobalMonad :: (ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, Monad m) => e -> GlobalMonad profile m a
    toGlobalMonad = toGlobalMonadImpl
    safeErrorTToGlobalMonad :: HasCallStack => SafeErrorT profile GlobalErrorP m a -> GlobalMonad profile m a
    safeErrorTToGlobalMonad = safeErrorTToGlobalMonadImpl
    globalLift :: (HasCallStack, MonadCatch m) => m a -> GlobalMonad profile m a
    globalLift = globalLiftImpl
    globalLiftUnliftIO :: (HasCallStack, MonadUnliftIO m) => m a -> GlobalMonad profile m a
    globalLiftUnliftIO = globalLiftUnliftIOImpl
    globalLiftIO :: (HasCallStack, MonadIO m, MonadCatch m) => IO a -> GlobalMonad profile m a
    globalLiftIO = globalLiftIOImpl
    globalLiftIOUnliftIO :: (HasCallStack, MonadUnliftIO m) => IO a -> GlobalMonad profile m a
    globalLiftIOUnliftIO = globalLiftIOUnliftIOImpl
    globalLog :: (HasCallStack, MonadError IOException m, MonadIO m, MonadCatch m) => Proxy profile -> Config -> IO a -> m a
    globalLog = globalLogImpl
    runGlobalErrorEither :: (HasCallStack, MonadError IOException m, MonadIO m, MonadCatch m) => Proxy profile -> Config -> Either GlobalInnerError a -> m a
    runGlobalErrorEither = runGlobalErrorEitherImpl
    runGlobalMonad :: (HasCallStack, MonadError IOException m, MonadIO m, MonadCatch m) => Config -> GlobalMonad profile IO a -> m a
    runGlobalMonad = runGlobalMonadImpl
    runGlobalMonadWithoutLog :: (HasCallStack, MonadError IOException m, MonadIO m, MonadCatch m) => GlobalMonad profile IO a -> m a
    runGlobalMonadWithoutLog = runGlobalMonadWithoutLogImpl
    maybeToGlobalMonad :: (HasCallStack, ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, Monad m) => Maybe a -> e -> GlobalMonad profile m a
    maybeToGlobalMonad= maybeToGlobalMonadImpl
    maybeTToGlobalMonad :: (HasCallStack, ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, MonadIO m, MonadCatch m) => MaybeT IO a -> e -> GlobalMonad profile m a
    maybeTToGlobalMonad = maybeTToGlobalMonadImpl
    maybeTToGlobalMonadUnliftIO :: (HasCallStack, ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, MonadUnliftIO m) => MaybeT IO a -> e -> GlobalMonad profile m a
    maybeTToGlobalMonadUnliftIO = maybeTToGlobalMonadUnliftIOImpl
    globalAssert :: (ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, Monad m) => Bool -> e -> GlobalMonad profile m ()
    globalAssert = globalAssertImpl
    globalCatch :: (HasCallStack, Monad m) => GlobalMonad profile m a -> (GlobalInnerError -> GlobalMonad profile m a) -> GlobalMonad profile m a
    globalCatch = globalCatchImpl

toGlobalMonadImpl :: forall e profile m a. (GlobalMonadI profile, ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, Monad m) => e -> GlobalMonad profile m a
toGlobalMonadImpl = GlobalMonad . lift . toSafeErrorT (Proxy :: Proxy e) . toInnerError

safeErrorTToGlobalMonadImpl :: (HasCallStack, GlobalMonadI profile) => SafeErrorT profile GlobalErrorP m a -> GlobalMonad profile m a
safeErrorTToGlobalMonadImpl = GlobalMonad . ProfileT . ReaderT . const

globalLiftImpl :: (HasCallStack, GlobalMonadI profile, MonadCatch m) => m a -> GlobalMonad profile m a
globalLiftImpl = GlobalMonad . lift . liftSafe

globalLiftUnliftIOImpl :: (HasCallStack, GlobalMonadI profile, MonadUnliftIO m) => m a -> GlobalMonad profile m a
globalLiftUnliftIOImpl = GlobalMonad . lift . liftSafeUnliftIO

globalLiftIOImpl :: (HasCallStack, GlobalMonadI profile, MonadIO m, MonadCatch m) => IO a -> GlobalMonad profile m a
globalLiftIOImpl = GlobalMonad . lift . liftIOSafe

globalLiftIOUnliftIOImpl :: (HasCallStack, GlobalMonadI profile, MonadUnliftIO m) => IO a -> GlobalMonad profile m a
globalLiftIOUnliftIOImpl = GlobalMonad . lift . liftIOSafeUnliftIO

globalLogImpl :: (HasCallStack, GlobalMonadI profile, MonadError IOException m, MonadIO m, MonadCatch m) => Proxy profile -> Config -> IO a -> m a
globalLogImpl profile cfg = errorLog profile (Proxy :: Proxy GlobalErrorP) cfg

runGlobalErrorEitherImpl :: (HasCallStack, GlobalMonadI profile, MonadError IOException m, MonadIO m, MonadCatch m) => Proxy profile -> Config -> Either GlobalInnerError a -> m a
runGlobalErrorEitherImpl profile cfg = runErrorEither profile (Proxy :: Proxy GlobalErrorP) cfg

runGlobalMonadImpl :: (HasCallStack, GlobalMonadI profile, MonadError IOException m, MonadIO m, MonadCatch m) => Config -> GlobalMonad profile IO a -> m a
runGlobalMonadImpl cfg (GlobalMonad (ProfileT (ReaderT x))) = runErrorT cfg $ x (Proxy :: Proxy p)

runGlobalMonadWithoutLogImpl :: (HasCallStack, GlobalMonadI profile, MonadError IOException m, MonadIO m, MonadCatch m) => GlobalMonad profile IO a -> m a
runGlobalMonadWithoutLogImpl (GlobalMonad (ProfileT (ReaderT x))) = runErrorTWithoutLog $ x (Proxy :: Proxy p)

maybeToGlobalMonadImpl :: (HasCallStack, GlobalMonadI profile, ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, Monad m) => Maybe a -> e -> GlobalMonad profile m a
maybeToGlobalMonadImpl a' ex = GlobalMonad $ lift $ maybeToErrorT a' ex

maybeTToGlobalMonadImpl :: (HasCallStack, GlobalMonadI profile, ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, MonadIO m, MonadCatch m) => MaybeT IO a -> e -> GlobalMonad profile m a
maybeTToGlobalMonadImpl m ex = GlobalMonad $ lift $ maybeTToErrorT m ex

maybeTToGlobalMonadUnliftIOImpl :: (HasCallStack, GlobalMonadI profile, ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, MonadUnliftIO m) => MaybeT IO a -> e -> GlobalMonad profile m a
maybeTToGlobalMonadUnliftIOImpl m ex = GlobalMonad $ lift $ maybeTToErrorTUnliftIO m ex

globalAssertImpl :: (GlobalMonadI profile, ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, Monad m) => Bool -> e -> GlobalMonad profile m ()
globalAssertImpl b ex = GlobalMonad $ lift $ errorAssert b ex

globalCatchImpl :: (HasCallStack, GlobalMonadI profile, Monad m) => GlobalMonad profile m a -> (GlobalInnerError -> GlobalMonad profile m a) -> GlobalMonad profile m a
globalCatchImpl (GlobalMonad (ProfileT (ReaderT f))) g =
    GlobalMonad $ ProfileT $ ReaderT $ (\profile ->
        errorCatch (f profile) (\ie ->
            (runReaderT $ unProfileT $ unGlobalMonad $ g ie) profile
        ))