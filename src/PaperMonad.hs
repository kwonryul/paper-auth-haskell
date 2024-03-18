{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module PaperMonad(
    PaperError(
        PaperError
      , paperErrorMsg
      , paperServerError
      , paperErrorCallStack
      )
  , PaperDefaultError(
        PaperDefaultError
      )
  , PaperCatchError(
        PaperCatchError
      )
  , PaperInnerError
  , PaperErrorP
  , PaperMonad(
        unPaperMonad
      )
  , PaperMonadI(
        toPaperMonad
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
      , paperCatch
      )
) where

import Monad.ErrorT
import Monad.ProfileT
import Import

import Servant

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Unlift
import Control.Monad.Catch
import Control.Monad.Error.Class
import Data.Kind
import GHC.Stack

data PaperError = PaperError {
    paperErrorMsg :: String
  , paperServerError :: ServerError
  , paperErrorCallStack :: CallStack
  }
instance Show PaperError where
    show (PaperError msg _ cs) = "[PaperError]\n" ++ msg ++ "\n\n" ++ prettyCallStack cs

data PaperDefaultError where
    PaperDefaultError :: Exception e => e -> CallStack -> PaperDefaultError
instance Show PaperDefaultError where
    show (PaperDefaultError ex cs) = "[PaperDefaultError]\n" ++ show ex ++ "\n" ++ prettyCallStack cs

data PaperCatchError where
    PaperCatchError :: Show e => e -> ServerError -> CallStack -> PaperCatchError
instance Show PaperCatchError where
    show (PaperCatchError ex _ cs) = "[PaperCatchError]\n" ++ show ex ++ "\n" ++ prettyCallStack cs

data PaperInnerError where
    PaperInnerError :: Show e => {
        paperInnerServerError :: ServerError
      , paperInnerLogError :: e
    } -> PaperInnerError
instance Show PaperInnerError where
    show (PaperInnerError { paperInnerLogError }) = show paperInnerLogError
instance Exception PaperInnerError

data PaperErrorP

type instance InnerError PaperError = PaperInnerError
type instance InnerError PaperDefaultError = PaperInnerError
type instance InnerError PaperCatchError = PaperInnerError
type instance OuterError PaperError = ServerError
type instance OuterError PaperDefaultError = ServerError
type instance OuterError PaperCatchError = ServerError
type instance DefaultError PaperErrorP = PaperDefaultError

instance ErrorTError PaperError where
    toOuterError _ (PaperInnerError { paperInnerServerError }) = paperInnerServerError
    toInnerError e = PaperInnerError {
        paperInnerServerError = paperServerError e
      , paperInnerLogError = e
      }

instance ErrorTError PaperDefaultError where
    toOuterError _ (PaperInnerError { paperInnerServerError }) = paperInnerServerError
    toInnerError e = PaperInnerError {
        paperInnerServerError = err500 { errBody = "unexpected server exception" }
      , paperInnerLogError = e
      }

instance ErrorTError PaperCatchError where
    toOuterError _ (PaperInnerError { paperInnerServerError }) = paperInnerServerError
    toInnerError ce@(PaperCatchError _ se _) = PaperInnerError {
        paperInnerServerError = se
      , paperInnerLogError = ce
      }

type PaperMonad :: Type -> (Type -> Type) -> Type -> Type
data PaperMonad profile m a where
    PaperMonad :: Profile profile => {
        unPaperMonad :: ProfileT profile (SafeErrorT profile PaperErrorP m) a
    } -> PaperMonad profile m a

instance (Profile profile, ErrorTProfile profile PaperErrorP, Functor m) => Functor (PaperMonad profile m) where
    fmap f (PaperMonad p) = PaperMonad $ fmap f p

instance (Profile profile, ErrorTProfile profile PaperErrorP, Monad m) => Applicative (PaperMonad profile m) where
    pure x = PaperMonad $ pure x
    PaperMonad f <*> PaperMonad x = PaperMonad $ f <*> x

instance (Profile profile, ErrorTProfile profile PaperErrorP, Monad m) => Monad (PaperMonad profile m) where
    return = pure
    PaperMonad x >>= f = PaperMonad $ x >>= unPaperMonad . f

instance (Profile profile, ErrorTProfile profile PaperErrorP, Monad m) => MonadReader (Proxy profile) (PaperMonad profile m) where
    ask = PaperMonad $ ask
    local f x = PaperMonad $ local f $ unPaperMonad x

instance (Profile profile, ErrorTProfile profile PaperErrorP, MonadIO m) => MonadLogger (PaperMonad profile m) where
    monadLoggerLog loc logSource logLevel msg = PaperMonad $ ProfileT $ ReaderT $ const $ monadLoggerLog loc logSource logLevel msg

class (ErrorTI profile, ErrorTProfile profile PaperErrorP) => PaperMonadI profile where
    toPaperMonad :: (ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, Monad m) => e -> PaperMonad profile m a
    toPaperMonad = toPaperMonadImpl
    safeErrorTToPaperMonad :: HasCallStack => SafeErrorT profile PaperErrorP m a -> PaperMonad profile m a
    safeErrorTToPaperMonad = safeErrorTToPaperMonadImpl
    paperLift :: (HasCallStack, MonadCatch m) => m a -> PaperMonad profile m a
    paperLift = paperLiftImpl
    paperLiftUnliftIO :: (HasCallStack, MonadUnliftIO m) => m a -> PaperMonad profile m a
    paperLiftUnliftIO = paperLiftUnliftIOImpl
    paperLiftIO :: (HasCallStack, MonadIO m, MonadCatch m) => IO a -> PaperMonad profile m a
    paperLiftIO = paperLiftIOImpl
    paperLiftIOUnliftIO :: (HasCallStack, MonadUnliftIO m) => IO a -> PaperMonad profile m a
    paperLiftIOUnliftIO = paperLiftIOUnliftIOImpl
    paperLog :: (HasCallStack, MonadError ServerError m, MonadIO m, MonadCatch m) => Proxy profile -> Import.Context -> IO a -> m a
    paperLog = paperLogImpl
    runPaperErrorEither :: (HasCallStack, MonadError ServerError m, MonadIO m, MonadCatch m) => Proxy profile -> Import.Context -> Either PaperInnerError a -> m a
    runPaperErrorEither = runPaperErrorEitherImpl
    runPaperMonad :: (HasCallStack, MonadError ServerError m, MonadIO m, MonadCatch m) => Import.Context -> PaperMonad profile IO a -> m a
    runPaperMonad = runPaperMonadImpl
    maybeToPaperMonad :: (HasCallStack, ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, Monad m) => Maybe a -> e -> PaperMonad profile m a
    maybeToPaperMonad = maybeToPaperMonadImpl
    maybeTToPaperMonad :: (HasCallStack, ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, MonadIO m, MonadCatch m) => MaybeT IO a -> e -> PaperMonad profile m a
    maybeTToPaperMonad = maybeTToPaperMonadImpl
    maybeTToPaperMonadUnliftIO :: (HasCallStack, ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, MonadUnliftIO m) => MaybeT IO a -> e -> PaperMonad profile m a
    maybeTToPaperMonadUnliftIO = maybeTToPaperMonadUnliftIOImpl
    paperAssert :: (ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, Monad m) => Bool -> e -> PaperMonad profile m ()
    paperAssert = paperAssertImpl
    paperCatch :: (HasCallStack, Monad m) => PaperMonad profile m a -> (PaperInnerError -> PaperMonad profile m a) -> PaperMonad profile m a
    paperCatch = paperCatchImpl

toPaperMonadImpl :: forall e profile m a. (PaperMonadI profile, ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, Monad m) => e -> PaperMonad profile m a
toPaperMonadImpl = PaperMonad . lift . toSafeErrorT (Proxy :: Proxy e). toInnerError

safeErrorTToPaperMonadImpl :: (HasCallStack, PaperMonadI profile) => SafeErrorT profile PaperErrorP m a -> PaperMonad profile m a
safeErrorTToPaperMonadImpl = PaperMonad . ProfileT . ReaderT . const

paperLiftImpl :: (HasCallStack, PaperMonadI profile, MonadCatch m) => m a -> PaperMonad profile m a
paperLiftImpl = PaperMonad . lift . liftSafe

paperLiftUnliftIOImpl :: (HasCallStack, PaperMonadI profile, MonadUnliftIO m) => m a -> PaperMonad profile m a
paperLiftUnliftIOImpl = PaperMonad . lift . liftSafeUnliftIO

paperLiftIOImpl :: (HasCallStack, PaperMonadI profile, MonadIO m, MonadCatch m) => IO a -> PaperMonad profile m a
paperLiftIOImpl = PaperMonad . lift . liftIOSafe

paperLiftIOUnliftIOImpl :: (HasCallStack, PaperMonadI profile, MonadUnliftIO m) => IO a -> PaperMonad profile m a
paperLiftIOUnliftIOImpl = PaperMonad . lift . liftIOSafeUnliftIO

paperLogImpl :: (HasCallStack, PaperMonadI profile, MonadError ServerError m, MonadIO m, MonadCatch m) => Proxy profile -> Import.Context -> IO a -> m a
paperLogImpl profile context = errorLog profile (Proxy :: Proxy PaperErrorP) context

runPaperErrorEitherImpl :: (HasCallStack, PaperMonadI profile, MonadError ServerError m, MonadIO m, MonadCatch m) => Proxy profile -> Import.Context -> Either PaperInnerError a -> m a
runPaperErrorEitherImpl profile context = runErrorEither profile (Proxy :: Proxy PaperErrorP) context

runPaperMonadImpl :: (HasCallStack, PaperMonadI profile, MonadError ServerError m, MonadIO m, MonadCatch m) => Import.Context -> PaperMonad profile IO a -> m a
runPaperMonadImpl context (PaperMonad (ProfileT (ReaderT x))) = runErrorT context $ x (Proxy :: Proxy profile)

maybeToPaperMonadImpl :: (HasCallStack, PaperMonadI profile, ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, Monad m) => Maybe a -> e -> PaperMonad profile m a
maybeToPaperMonadImpl a' ex = PaperMonad $ lift $ maybeToErrorT a' ex

maybeTToPaperMonadImpl :: (HasCallStack, PaperMonadI profile, ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, MonadIO m, MonadCatch m) => MaybeT IO a -> e -> PaperMonad profile m a
maybeTToPaperMonadImpl m ex = PaperMonad $ lift $ maybeTToErrorT m ex

maybeTToPaperMonadUnliftIOImpl :: (HasCallStack, PaperMonadI profile, ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, MonadUnliftIO m) => MaybeT IO a -> e -> PaperMonad profile m a
maybeTToPaperMonadUnliftIOImpl m ex = PaperMonad $ lift $ maybeTToErrorTUnliftIO m ex

paperAssertImpl :: (PaperMonadI profile, ErrorTError e, InnerError e ~ PaperInnerError, OuterError e ~ ServerError, Monad m) => Bool -> e -> PaperMonad profile m ()
paperAssertImpl b ex = PaperMonad $ lift $ errorAssert b ex

paperCatchImpl :: (HasCallStack, PaperMonadI profile, Monad m) => PaperMonad profile m a -> (PaperInnerError -> PaperMonad profile m a) -> PaperMonad profile m a
paperCatchImpl (PaperMonad (ProfileT (ReaderT f))) g =
    PaperMonad $ ProfileT $ ReaderT $ (\profile ->
        errorCatch (f profile) (\ie ->
            (runReaderT $ unProfileT $ unPaperMonad $ g ie) profile
        ))