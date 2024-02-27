module GlobalError(
    GlobalError(GlobalError)
  , ToGlobalError(
        toGlobalError
      , toGlobalExceptT
      )
  , GlobalEither
  , GlobalExceptT
  , globalLift
  , unsafeGlobalExceptTToSafe
  , globalLiftIO
  , runGlobalEither
  , runGlobalExceptT
  , globalLog
  , maybeToGlobalEither
  , maybeTToGlobalExceptT
  , globalAssert
  , GlobalException(GlobalException)
) where

import CallStack

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Exception
import UnliftIO.Exception
import Data.Time
import GHC.Stack

data GlobalError where
    GlobalError :: Show e => e -> GlobalError

instance Show GlobalError where
    show (GlobalError e) = show e

instance Exception GlobalError

class Show e => ToGlobalError e where
    toGlobalError :: e -> GlobalError
    toGlobalError e = GlobalError e
    toGlobalExceptT :: Monad m => e -> GlobalExceptT m a
    toGlobalExceptT = ExceptT . return . Left . toGlobalError

type GlobalEither = Either GlobalError
type GlobalExceptT = ExceptT GlobalError

data GlobalErrorWithUTC = GlobalErrorWithUTC GlobalError UTCTime

instance Show GlobalErrorWithUTC where
    show (GlobalErrorWithUTC ex time) =
        "[GlobalError]\t"
        ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
        ++ "\n"
        ++ show ex

instance Exception GlobalErrorWithUTC

instance {-# OVERLAPPING #-} MonadIO m => MonadIO (GlobalExceptT m) where
    liftIO io = ExceptT $ liftIO $ Right <$> io

instance MonadUnliftIO m => MonadUnliftIO (GlobalExceptT m) where
    withRunInIO inner = ExceptT $ UnliftIO.Exception.catch (Right <$> withRunInIO (\f -> inner (f . unwrap))) (\(g :: GlobalError) ->
        return $ Left $ g
        )
        where
            unwrap :: MonadUnliftIO m => GlobalExceptT m a -> m a
            unwrap (ExceptT mga) = do
                ga <- mga
                case ga of
                    Right a -> return a
                    Left g -> UnliftIO.Exception.throwIO g

globalLift :: (HasCallStack, MonadUnliftIO m) => m a -> GlobalExceptT m a
globalLift ma = ExceptT $ UnliftIO.Exception.catch (Right <$> ma) (\(ex :: SomeException) ->
    return $ Left $ toGlobalError $ GlobalOuterException ex callStack')

unsafeGlobalExceptTToSafe :: (HasCallStack, MonadUnliftIO m) => GlobalExceptT m a -> GlobalExceptT m a
unsafeGlobalExceptTToSafe (ExceptT mga) = do
    ga <- globalLift mga
    case ga of
        Right a -> return a
        Left ex -> ExceptT $ return $ Left ex

globalLiftIO :: (HasCallStack, MonadUnliftIO m) => IO a -> GlobalExceptT m a
globalLiftIO = globalLift . liftIO

runGlobalEither :: MonadIO m => GlobalEither a -> m a
runGlobalEither g = case g of
    Left ex -> liftIO $ do
        currentTime <- getCurrentTime
        Control.Exception.throwIO $ GlobalErrorWithUTC ex currentTime
    Right x -> return x

runGlobalExceptT :: MonadIO m => GlobalExceptT m a -> m a
runGlobalExceptT g = do
    a' <- runExceptT g
    runGlobalEither a'

globalLog :: HasCallStack => IO a -> IO a
globalLog io = Control.Exception.catch io (\(ex :: SomeException) -> do
    currentTime <- getCurrentTime
    Control.Exception.throwIO $ GlobalErrorWithUTC (toGlobalError $ GlobalOuterException ex callStack') currentTime
    )

maybeToGlobalEither :: ToGlobalError g => Maybe a -> g -> GlobalEither a
maybeToGlobalEither a' ex = case a' of
    Just a -> Right a
    Nothing -> Left $ toGlobalError ex

maybeTToGlobalExceptT :: (ToGlobalError g, MonadUnliftIO m) => MaybeT IO a -> g -> GlobalExceptT m a
maybeTToGlobalExceptT (MaybeT ima) ex = do
    a' <- globalLiftIO ima
    case a' of
        Just a -> return a
        Nothing -> toGlobalExceptT ex

globalAssert :: (ToGlobalError g, Monad m) => Bool -> g -> GlobalExceptT m ()
globalAssert b g =
    if b then
        return ()
    else
        toGlobalExceptT g

data GlobalOuterException where
    GlobalOuterException :: Exception e => e -> CallStack ->  GlobalOuterException

instance Show GlobalOuterException where
    show (GlobalOuterException ex cs) = "[GlobalOuterException]\n" ++ show ex ++ "\n" ++ prettyCallStack cs

instance ToGlobalError GlobalOuterException where
    toGlobalError goe = GlobalError goe

data GlobalException = GlobalException String CallStack

instance Show GlobalException where
    show (GlobalException msg cs) = "[GlobalException]\n" ++ msg ++ "\n" ++ prettyCallStack cs

instance ToGlobalError GlobalException