{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module NestedMonad(
    NestedError(
        NestedError
      , nestedErrorMsg
      , nestedErrorCallStack
      )
  , NestedDefaultError(
        NestedDefaultError
      )
  , NestedCatchError(
        NestedCatchError
      )
  , NestedInnerError
  , NestedErrorP
  , NestedMonad(
        unNestedMonad
      )
  , NestedMonadI(
        toNestedMonad
      , safeErrorTToNestedMonad
      , nestedLift
      , nestedLiftUnliftIO
      , nestedLiftIO
      , nestedLiftIOUnliftIO
      , nestedLog
      , runNestedErrorEither
      , runNestedMonad
      , runNestedMonadWithoutLog
      , maybeToNestedMonad
      , maybeTToNestedMonad
      , maybeTToNestedMonadUnliftIO
      , nestedAssert
      , nestedCatch
      )
) where

import Monad.ErrorT
import Monad.ProfileT
import Import

import Servant

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Unlift
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Exception
import Data.Kind
import GHC.Stack

data NestedError = NestedError {
    nestedErrorMsg :: String
  , nestedErrorCallStack :: CallStack
  }
instance Show NestedError where
    show (NestedError msg cs) = "[NestedError]\n" ++ msg ++ "\n\n" ++ prettyCallStack cs

data NestedDefaultError where
    NestedDefaultError :: Exception e => e -> CallStack -> NestedDefaultError
instance Show NestedDefaultError where
    show (NestedDefaultError ex cs) = "[NestedDefaultError]\n" ++ show ex ++ "\n" ++ prettyCallStack cs

data NestedCatchError where
    NestedCatchError :: Show e => e -> CallStack -> NestedCatchError
instance Show NestedCatchError where
    show (NestedCatchError ex cs) = "[NestedCatchError]\n" ++ show ex ++ "\n" ++ prettyCallStack cs

data NestedInnerError where
    NestedInnerError :: Show e => e -> NestedInnerError
instance Show NestedInnerError where
    show (NestedInnerError e) = show e
instance Exception NestedInnerError

data NestedErrorP

type instance InnerError NestedError = NestedInnerError
type instance InnerError NestedDefaultError = NestedInnerError
type instance OuterError NestedError = IOException
type instance OuterError NestedDefaultError = IOException
type instance DefaultError NestedErrorP = NestedDefaultError

instance ErrorTError NestedError where
    toOuterError _ ie = userError $ show ie
    toInnerError e = NestedInnerError e
instance ErrorTError NestedDefaultError where
    toOuterError _ ie = userError $ show ie
    toInnerError e = NestedInnerError e

type NestedMonad :: Type -> (Type -> Type) -> Type -> Type
data NestedMonad profile m a where
    NestedMonad :: Profile profile => {
        unNestedMonad :: ProfileT profile (SafeErrorT profile NestedErrorP m) a
    } -> NestedMonad profile m a

instance (Profile profile, ErrorTProfile profile NestedErrorP, Functor m) => Functor (NestedMonad profile m) where
    fmap f (NestedMonad p) = NestedMonad $ fmap f p

instance (Profile profile, ErrorTProfile profile NestedErrorP, Monad m) => Applicative (NestedMonad profile m) where
    pure x = NestedMonad $ pure x
    NestedMonad f <*> NestedMonad x = NestedMonad $ f <*> x

instance (Profile profile, ErrorTProfile profile NestedErrorP, Monad m) => Monad (NestedMonad profile m) where
    return = pure
    NestedMonad x >>= f = NestedMonad $ x >>= unNestedMonad . f

instance (Profile profile, ErrorTProfile profile NestedErrorP, Monad m) => MonadReader (Proxy profile) (NestedMonad profile m) where
    ask = NestedMonad $ ask
    local f x = NestedMonad $ local f $ unNestedMonad x

class (ErrorTI profile, ErrorTProfile profile NestedErrorP) => NestedMonadI profile where
    toNestedMonad :: forall e m a. (ErrorTError e, InnerError e ~ NestedInnerError, OuterError e ~ IOException, Monad m) => e -> NestedMonad profile m a
    toNestedMonad = toNestedMonadImpl
    safeErrorTToNestedMonad :: HasCallStack => SafeErrorT profile NestedErrorP m a -> NestedMonad profile m a
    safeErrorTToNestedMonad = safeErrorTToNestedMonadImpl
    nestedLift :: (HasCallStack, MonadCatch m) => m a -> NestedMonad profile m a
    nestedLift = nestedLiftImpl
    nestedLiftUnliftIO :: (HasCallStack, MonadUnliftIO m) => m a -> NestedMonad profile m a
    nestedLiftUnliftIO = nestedLiftUnliftIOImpl
    nestedLiftIO :: (HasCallStack, MonadIO m, MonadCatch m) => IO a -> NestedMonad profile m a
    nestedLiftIO = nestedLiftIOImpl
    nestedLiftIOUnliftIO :: (HasCallStack, MonadUnliftIO m) => IO a -> NestedMonad profile m a
    nestedLiftIOUnliftIO = nestedLiftIOUnliftIOImpl
    nestedLog :: (HasCallStack, MonadError IOException m, MonadIO m, MonadCatch m) => Proxy profile -> Import.Context -> IO a -> m a
    nestedLog = nestedLogImpl
    runNestedErrorEither :: (HasCallStack, MonadError IOException m, MonadIO m, MonadCatch m) => Proxy profile -> Import.Context -> Either NestedInnerError a -> m a
    runNestedErrorEither = runNestedErrorEitherImpl
    runNestedMonad :: (HasCallStack, MonadError IOException m, MonadIO m, MonadCatch m) => Import.Context -> NestedMonad profile IO a -> m a
    runNestedMonad = runNestedMonadImpl
    runNestedMonadWithoutLog :: (HasCallStack, MonadError IOException m, MonadIO m, MonadCatch m) => NestedMonad profile IO a -> m a
    runNestedMonadWithoutLog = runNestedMonadWithoutLogImpl
    maybeToNestedMonad :: (HasCallStack, ErrorTError e, InnerError e ~ NestedInnerError, OuterError e ~ IOException, Monad m) => Maybe a -> e -> NestedMonad profile m a
    maybeToNestedMonad= maybeToNestedMonadImpl
    maybeTToNestedMonad :: (HasCallStack, ErrorTError e, InnerError e ~ NestedInnerError, OuterError e ~ IOException, MonadIO m, MonadCatch m) => MaybeT IO a -> e -> NestedMonad profile m a
    maybeTToNestedMonad = maybeTToNestedMonadImpl
    maybeTToNestedMonadUnliftIO :: (HasCallStack, ErrorTError e, InnerError e ~ NestedInnerError, OuterError e ~ IOException, MonadUnliftIO m) => MaybeT IO a -> e -> NestedMonad profile m a
    maybeTToNestedMonadUnliftIO = maybeTToNestedMonadUnliftIOImpl
    nestedAssert :: (ErrorTError e, InnerError e ~ NestedInnerError, OuterError e ~ IOException, Monad m) => Bool -> e -> NestedMonad profile m ()
    nestedAssert = nestedAssertImpl
    nestedCatch :: (HasCallStack, Monad m) => NestedMonad profile m a -> (NestedInnerError -> NestedMonad profile m a) -> NestedMonad profile m a
    nestedCatch = nestedCatchImpl

toNestedMonadImpl :: forall e profile m a. (NestedMonadI profile, ErrorTError e, InnerError e ~ NestedInnerError, OuterError e ~ IOException, Monad m) => e -> NestedMonad profile m a
toNestedMonadImpl = NestedMonad . lift . toSafeErrorT (Proxy :: Proxy e) . toInnerError

safeErrorTToNestedMonadImpl :: (HasCallStack, NestedMonadI profile) => SafeErrorT profile NestedErrorP m a -> NestedMonad profile m a
safeErrorTToNestedMonadImpl = NestedMonad . ProfileT . ReaderT . const

nestedLiftImpl :: (HasCallStack, NestedMonadI profile, MonadCatch m) => m a -> NestedMonad profile m a
nestedLiftImpl = NestedMonad . lift . liftSafe

nestedLiftUnliftIOImpl :: (HasCallStack, NestedMonadI profile, MonadUnliftIO m) => m a -> NestedMonad profile m a
nestedLiftUnliftIOImpl = NestedMonad . lift . liftSafeUnliftIO

nestedLiftIOImpl :: (HasCallStack, NestedMonadI profile, MonadIO m, MonadCatch m) => IO a -> NestedMonad profile m a
nestedLiftIOImpl = NestedMonad . lift . liftIOSafe

nestedLiftIOUnliftIOImpl :: (HasCallStack, NestedMonadI profile, MonadUnliftIO m) => IO a -> NestedMonad profile m a
nestedLiftIOUnliftIOImpl = NestedMonad . lift . liftIOSafeUnliftIO

nestedLogImpl :: (HasCallStack, NestedMonadI profile, MonadError IOException m, MonadIO m, MonadCatch m) => Proxy profile -> Import.Context -> IO a -> m a
nestedLogImpl profile context = errorLog profile (Proxy :: Proxy NestedErrorP) context

runNestedErrorEitherImpl :: (HasCallStack, NestedMonadI profile, MonadError IOException m, MonadIO m, MonadCatch m) => Proxy profile -> Import.Context -> Either NestedInnerError a -> m a
runNestedErrorEitherImpl profile context = runErrorEither profile (Proxy :: Proxy NestedErrorP) context

runNestedMonadImpl :: (HasCallStack, NestedMonadI profile, MonadError IOException m, MonadIO m, MonadCatch m) => Import.Context -> NestedMonad profile IO a -> m a
runNestedMonadImpl context (NestedMonad (ProfileT (ReaderT x))) = runErrorT context $ x (Proxy :: Proxy p)

runNestedMonadWithoutLogImpl :: (HasCallStack, NestedMonadI profile, MonadError IOException m, MonadIO m, MonadCatch m) => NestedMonad profile IO a -> m a
runNestedMonadWithoutLogImpl (NestedMonad (ProfileT (ReaderT x))) = runErrorTWithoutLog $ x (Proxy :: Proxy p)

maybeToNestedMonadImpl :: (HasCallStack, NestedMonadI profile, ErrorTError e, InnerError e ~ NestedInnerError, OuterError e ~ IOException, Monad m) => Maybe a -> e -> NestedMonad profile m a
maybeToNestedMonadImpl a' ex = NestedMonad $ lift $ maybeToErrorT a' ex

maybeTToNestedMonadImpl :: (HasCallStack, NestedMonadI profile, ErrorTError e, InnerError e ~ NestedInnerError, OuterError e ~ IOException, MonadIO m, MonadCatch m) => MaybeT IO a -> e -> NestedMonad profile m a
maybeTToNestedMonadImpl m ex = NestedMonad $ lift $ maybeTToErrorT m ex

maybeTToNestedMonadUnliftIOImpl :: (HasCallStack, NestedMonadI profile, ErrorTError e, InnerError e ~ NestedInnerError, OuterError e ~ IOException, MonadUnliftIO m) => MaybeT IO a -> e -> NestedMonad profile m a
maybeTToNestedMonadUnliftIOImpl m ex = NestedMonad $ lift $ maybeTToErrorTUnliftIO m ex

nestedAssertImpl :: (NestedMonadI profile, ErrorTError e, InnerError e ~ NestedInnerError, OuterError e ~ IOException, Monad m) => Bool -> e -> NestedMonad profile m ()
nestedAssertImpl b ex = NestedMonad $ lift $ errorAssert b ex

nestedCatchImpl :: (HasCallStack, NestedMonadI profile, Monad m) => NestedMonad profile m a -> (NestedInnerError -> NestedMonad profile m a) -> NestedMonad profile m a
nestedCatchImpl (NestedMonad (ProfileT (ReaderT f))) g =
    NestedMonad $ ProfileT $ ReaderT $ (\profile ->
        errorCatch (f profile) (\ie ->
            (runReaderT $ unProfileT $ unNestedMonad $ g ie) profile
        ))