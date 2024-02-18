module Exception(
    PaperEither
  , PaperEitherT
  , toPaperEither
  , toPaperEitherT
  , toPaperEitherT'
  , liftIOEitherT
  , liftIOEitherT'
  , PaperException(
      ConfigMissing
    , DatabaseInitializeException
  )
) where

import Import

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either

type PaperEither = Either PaperException
type PaperEitherT = EitherT PaperException

toPaperEither :: Maybe a -> PaperException -> PaperEither a
toPaperEither a' ex =
    case a' of
        Just a -> Right a
        Nothing -> Left ex

toPaperEitherT :: Monad m => MaybeT m a -> PaperException -> PaperEitherT m a
toPaperEitherT mma = toPaperEitherT' $ runMaybeT mma

toPaperEitherT' :: Monad m => m (Maybe a) -> PaperException -> PaperEitherT m a
toPaperEitherT' mma ex =
    EitherT $ do
        a' <- mma
        case a' of
            Just a -> return $ Right a
            Nothing -> return $ Left ex

liftIOEitherT :: MonadIO m => PaperEitherT IO a -> PaperEitherT m a
liftIOEitherT pia = liftIOEitherT' $ runEitherT pia

liftIOEitherT' :: MonadIO m => IO (Either PaperException a) -> PaperEitherT m a
liftIOEitherT' iea = EitherT $ liftIO iea

data PaperException where
    ConfigMissing :: String -> PaperException
    DatabaseInitializeException :: (Show db, DB db) => db -> PaperException

instance Show PaperException where
    show (ConfigMissing name) = "[ConfigMissing]\t" ++ name
    show (DatabaseInitializeException db) = "[DatabaseInitializeException]\t" ++ show db

instance Exception PaperException