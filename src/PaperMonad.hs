{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module PaperMonad(
    PaperError(
        PaperError
      , paperErrorMsg
      , paperServerError
      , paperErrorCallStack
      )
  , PaperDefaultError
  , PaperInnerError
  , PaperErrorP
  , PaperMonad(
        unPaperMonad
      )
  , toPaperMonad
  , safeErrorTToPaperMonad
  , paperLift
  , paperLiftUnliftIO
  , paperLiftIO
  , paperLiftIOUnliftIO
  , paperLog
  , runPaperErrorEither
  , runPaperMonad
  , maybeToPaperMonad
  , maybeTToPaperMonad
  , maybeTToPaperMonadUnliftIO
  , paperAssert
) where

import Monad.ErrorT
import Monad.ProfileT
import Paths_paper_auth

import Servant

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Unlift
import Control.Monad.Catch
import Control.Monad.Error.Class
import Data.Kind
import Data.Time
import Data.Text.Encoding
import Data.Text.IO
import GHC.Stack

data PaperError = PaperError {
    paperErrorMsg :: String
  , paperServerError :: ServerError
  , paperErrorCallStack :: CallStack
  }
instance Show PaperError where
    show (PaperError msg _ cs) = "[PaperError]\n" ++ show msg ++ "\n" ++ prettyCallStack cs

data PaperDefaultError where
    PaperDefaultError :: Exception e => e -> CallStack -> PaperDefaultError
instance Show PaperDefaultError where
    show (PaperDefaultError ex cs) = "[PaperDefaultError]\n" ++ show ex ++ "\n" ++ prettyCallStack cs

data PaperInnerError where
    PaperInnerError :: Show e => {
        paperInnerServerError :: ServerError
      , paperInnerLogError :: e
    } -> PaperInnerError
instance Show PaperInnerError where
    show (PaperInnerError { paperInnerLogError }) = show paperInnerLogError
instance Exception PaperInnerError

instance ErrorTError PaperError where
    type InnerError PaperError = PaperInnerError
    type OuterError PaperError = ServerError
    toOuterError _ (PaperInnerError { paperInnerServerError }) = paperInnerServerError
    toInnerError e = PaperInnerError {
        paperInnerServerError = paperServerError e
      , paperInnerLogError = e
      }

instance ErrorTError PaperDefaultError where
    type InnerError PaperDefaultError = PaperInnerError
    type OuterError PaperDefaultError = ServerError
    toOuterError _ (PaperInnerError { paperInnerServerError }) = paperInnerServerError
    toInnerError e = PaperInnerError {
        paperInnerServerError = err500 { errBody = "Unexpected server exception" }
      , paperInnerLogError = e
      }

data PaperErrorP

instance ErrorTProfile PaperErrorP where
    type DefaultError PaperErrorP = PaperDefaultError
    defaultError = PaperDefaultError
    defaultLogger _ = (\_ _ _ msg  -> do
        let filePath = "/home/kwonryul/dev/haskell/paper-auth/log/error.log"
        Data.Text.IO.appendFile filePath (Data.Text.Encoding.decodeUtf8 $ fromLogStr msg)
        )
    defaultErrorLog _ ie currentTime = (defaultLoc, "PaperError", LevelError, paperLogStr ie currentTime)

paperLogStr :: PaperInnerError -> UTCTime -> LogStr
paperLogStr ie currentTime =
    let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime in
    toLogStr $ "[Paper]\t" ++ formattedDate ++ "\n" ++ show ie ++ "\n"

type PaperMonad :: Type -> (Type -> Type) -> Type -> Type
data PaperMonad p m a where
    PaperMonad :: Profile p => {
        unPaperMonad :: ProfileT p (SafeErrorT PaperErrorP m) a
    } -> PaperMonad p m a

instance (Profile p, Functor m) => Functor (PaperMonad p m) where
    fmap f (PaperMonad p) = PaperMonad $ fmap f p

instance (Profile p, Monad m) => Applicative (PaperMonad p m) where
    pure x = PaperMonad $ pure x
    PaperMonad f <*> PaperMonad x = PaperMonad $ f <*> x

instance (Profile p, Monad m) => Monad (PaperMonad p m) where
    return = pure
    PaperMonad x >>= f = PaperMonad $ x >>= unPaperMonad . f

instance (Profile p, Monad m) => MonadReader (Proxy p) (PaperMonad p m) where
    ask = PaperMonad $ ask
    local f x = PaperMonad $ local f $ unPaperMonad x

instance (Profile p, MonadIO m) => MonadLogger (PaperMonad p m) where
    monadLoggerLog loc logSource logLevel msg = PaperMonad $ ProfileT $ ReaderT $ const $ monadLoggerLog loc logSource logLevel msg

toPaperMonad :: forall e p m a. (Profile p, ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, Monad m) => e -> PaperMonad p m a
toPaperMonad = PaperMonad . lift . toSafeErrorT (Proxy :: Proxy e). toInnerError

safeErrorTToPaperMonad :: (HasCallStack, Profile p) => SafeErrorT PaperErrorP m a -> PaperMonad p m a
safeErrorTToPaperMonad = PaperMonad . ProfileT . ReaderT . const

paperLift :: (HasCallStack, Profile p, MonadCatch m) => m a -> PaperMonad p m a
paperLift = PaperMonad . lift . liftSafe

paperLiftUnliftIO :: (HasCallStack, Profile p, MonadUnliftIO m) => m a -> PaperMonad p m a
paperLiftUnliftIO = PaperMonad . lift . liftSafeUnliftIO

paperLiftIO :: (HasCallStack, Profile p, MonadIO m, MonadCatch m) => IO a -> PaperMonad p m a
paperLiftIO = PaperMonad . lift . liftIOSafe

paperLiftIOUnliftIO :: (HasCallStack, Profile p, MonadUnliftIO m) => IO a -> PaperMonad p m a
paperLiftIOUnliftIO = PaperMonad . lift . liftIOSafeUnliftIO

paperLog :: (HasCallStack, MonadError ServerError m, MonadIO m, MonadCatch m) => IO a -> m a
paperLog = errorLog (Proxy :: Proxy PaperErrorP)

runPaperErrorEither :: (HasCallStack, MonadError ServerError m, MonadIO m, MonadCatch m) => Either PaperInnerError a -> m a
runPaperErrorEither = runErrorEither (Proxy :: Proxy PaperErrorP)

runPaperMonad :: (HasCallStack, Profile p, MonadError ServerError m, MonadIO m, MonadCatch m) => PaperMonad p IO a -> m a
runPaperMonad (PaperMonad (ProfileT (ReaderT x))) = runErrorT $ x (Proxy :: Proxy p)

maybeToPaperMonad :: (HasCallStack, Profile p, ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, Monad m) => Maybe a -> e -> PaperMonad p m a
maybeToPaperMonad a' ex = PaperMonad $ lift $ maybeToErrorT a' ex

maybeTToPaperMonad :: (HasCallStack, Profile p, ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, MonadIO m, MonadCatch m) => MaybeT IO a -> e -> PaperMonad p m a
maybeTToPaperMonad m ex = PaperMonad $ lift $ maybeTToErrorT m ex

maybeTToPaperMonadUnliftIO :: (HasCallStack, Profile p, ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, MonadUnliftIO m) => MaybeT IO a -> e -> PaperMonad p m a
maybeTToPaperMonadUnliftIO m ex = PaperMonad $ lift $ maybeTToErrorTUnliftIO m ex

paperAssert :: (Profile p, ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, Monad m) => Bool -> e -> PaperMonad p m ()
paperAssert b ex = PaperMonad $ lift $ errorAssert b ex