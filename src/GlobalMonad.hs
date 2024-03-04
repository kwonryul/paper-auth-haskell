{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module GlobalMonad(
    GlobalError(
        GlobalError
      , globalErrorMsg
      , globalErrorCallStack
      )
  , GlobalDefaultError
  , GlobalInnerError
  , GlobalErrorP
  , GlobalMonad(
        unGlobalMonad
      )
  , toGlobalMonad
  , safeErrorTToGlobalMonad
  , globalLift
  , globalLiftUnliftIO
  , globalLiftIO
  , globalLiftIOUnliftIO
  , globalLog
  , runGlobalErrorEither
  , runGlobalMonad
  , maybeToGlobalMonad
  , maybeTToGlobalMonad
  , maybeTToGlobalMonadUnliftIO
  , globalAssert
) where

import Monad.ErrorT
import Monad.ProfileT

import Servant

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Unlift
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Exception
import Data.Kind
import Data.Time
import Data.Text.Encoding
import Data.Text.IO
import GHC.Stack

data GlobalError = GlobalError {
    globalErrorMsg :: String
  , globalErrorCallStack :: CallStack
  }
instance Show GlobalError where
    show (GlobalError msg cs) = "[GlobalError]\n" ++ show msg ++ "\n" ++ prettyCallStack cs

data GlobalDefaultError where
    GlobalDefaultError :: Exception e => e -> CallStack -> GlobalDefaultError
instance Show GlobalDefaultError where
    show (GlobalDefaultError ex cs) = "[GlobalDefaultError]\n" ++ show ex ++ "\n" ++ prettyCallStack cs

data GlobalInnerError where
    GlobalInnerError :: Show e => e -> GlobalInnerError
instance Show GlobalInnerError where
    show (GlobalInnerError e) = show e
instance Exception GlobalInnerError

instance ErrorTError GlobalError where
    type InnerError GlobalError = GlobalInnerError
    type OuterError GlobalError = IOException
    toOuterError _ ie = userError $ show ie
    toInnerError e = GlobalInnerError e

instance ErrorTError GlobalDefaultError where
    type InnerError GlobalDefaultError = GlobalInnerError
    type OuterError GlobalDefaultError = IOException
    toOuterError _ ie = userError $ show ie
    toInnerError e = GlobalInnerError e

data GlobalErrorP

instance ErrorTProfile GlobalErrorP where
    type DefaultError GlobalErrorP = GlobalDefaultError
    defaultError = GlobalDefaultError
    defaultLogger _ = (\_ _ _ msg  -> do
        let filePath = "/home/kwonryul/dev/haskell/paper-auth/log/error.log"
        Data.Text.IO.appendFile filePath (Data.Text.Encoding.decodeUtf8 $ fromLogStr msg)
        )
    defaultErrorLog _ ie currentTime = (defaultLoc, "GlobalError", LevelError, globalLogStr ie currentTime)

globalLogStr :: GlobalInnerError -> UTCTime -> LogStr
globalLogStr ie currentTime =
    let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime in
    toLogStr $ "[Global]\t" ++ formattedDate ++ "\n" ++ show ie ++ "\n"

type GlobalMonad :: Type -> (Type -> Type) -> Type -> Type
data GlobalMonad p m a where
    GlobalMonad :: Profile p => {
        unGlobalMonad :: ProfileT p (SafeErrorT GlobalErrorP m) a
    } -> GlobalMonad p m a

instance (Profile p, Functor m) => Functor (GlobalMonad p m) where
    fmap f (GlobalMonad p) = GlobalMonad $ fmap f p

instance (Profile p, Monad m) => Applicative (GlobalMonad p m) where
    pure x = GlobalMonad $ pure x
    GlobalMonad f <*> GlobalMonad x = GlobalMonad $ f <*> x

instance (Profile p, Monad m) => Monad (GlobalMonad p m) where
    return = pure
    GlobalMonad x >>= f = GlobalMonad $ x >>= unGlobalMonad . f

instance (Profile p, Monad m) => MonadReader (Proxy p) (GlobalMonad p m) where
    ask = GlobalMonad $ ask
    local f x = GlobalMonad $ local f $ unGlobalMonad x

toGlobalMonad :: forall e p m a. (Profile p, ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, Monad m) => e -> GlobalMonad p m a
toGlobalMonad = GlobalMonad . lift . toSafeErrorT (Proxy :: Proxy e) . toInnerError

safeErrorTToGlobalMonad :: (HasCallStack, Profile p) => SafeErrorT GlobalErrorP m a -> GlobalMonad p m a
safeErrorTToGlobalMonad = GlobalMonad . ProfileT . ReaderT . const

globalLift :: (HasCallStack, Profile p, MonadCatch m) => m a -> GlobalMonad p m a
globalLift = GlobalMonad . lift . liftSafe

globalLiftUnliftIO :: (HasCallStack, Profile p, MonadUnliftIO m) => m a -> GlobalMonad p m a
globalLiftUnliftIO = GlobalMonad . lift . liftSafeUnliftIO

globalLiftIO :: (HasCallStack, Profile p, MonadIO m, MonadCatch m) => IO a -> GlobalMonad p m a
globalLiftIO = GlobalMonad . lift . liftIOSafe

globalLiftIOUnliftIO :: (HasCallStack, Profile p, MonadUnliftIO m) => IO a -> GlobalMonad p m a
globalLiftIOUnliftIO = GlobalMonad . lift . liftIOSafeUnliftIO

globalLog :: (HasCallStack, MonadError IOException m, MonadIO m, MonadCatch m) => IO a -> m a
globalLog = errorLog (Proxy :: Proxy GlobalErrorP)

runGlobalErrorEither :: (HasCallStack, MonadError IOException m, MonadIO m, MonadCatch m) => Either GlobalInnerError a -> m a
runGlobalErrorEither = runErrorEither (Proxy :: Proxy GlobalErrorP)

runGlobalMonad :: (HasCallStack, Profile p, MonadError IOException m, MonadIO m, MonadCatch m) => GlobalMonad p IO a -> m a
runGlobalMonad (GlobalMonad (ProfileT (ReaderT x))) = runErrorT $ x (Proxy :: Proxy p)

maybeToGlobalMonad :: (HasCallStack, Profile p, ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, Monad m) => Maybe a -> e -> GlobalMonad p m a
maybeToGlobalMonad a' ex = GlobalMonad $ lift $ maybeToErrorT a' ex

maybeTToGlobalMonad :: (HasCallStack, Profile p, ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, MonadIO m, MonadCatch m) => MaybeT IO a -> e -> GlobalMonad p m a
maybeTToGlobalMonad m ex = GlobalMonad $ lift $ maybeTToErrorT m ex

maybeTToGlobalMonadUnliftIO :: (HasCallStack, Profile p, ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, MonadUnliftIO m) => MaybeT IO a -> e -> GlobalMonad p m a
maybeTToGlobalMonadUnliftIO m ex = GlobalMonad $ lift $ maybeTToErrorTUnliftIO m ex

globalAssert :: (Profile p, ErrorTError e, InnerError e ~ GlobalInnerError, OuterError e ~ IOException, Monad m) => Bool -> e -> GlobalMonad p m ()
globalAssert b ex = GlobalMonad $ lift $ errorAssert b ex