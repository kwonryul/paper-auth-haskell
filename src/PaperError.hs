{-# LANGUAGE OverloadedStrings #-}

module PaperError(
    PaperError(
        PaperError
      , paperServerError
      , paperLogError
      )
  , ToPaperError(
        toPaperError
      , toPaperExceptT
      )
  , PaperEither
  , PaperExceptT
  , paperLift
  , unsafePaperExceptTToSafe
  , paperLiftIO
  , runPaperEither
  , runPaperExceptT
  , paperLog
  , maybeToPaperEither
  , maybeTToPaperExceptT
  , paperAssert
  , PaperException(PaperException)
) where

import CallStack

import Servant

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Catch
import UnliftIO.Exception
import Data.Time
import GHC.Stack

data PaperError where
    PaperError :: Show e => {
        paperServerError :: ServerError
      , paperLogError :: e
    } -> PaperError

instance Show PaperError where
    show (PaperError { paperLogError }) = show paperLogError

instance Exception PaperError

class Show e => ToPaperError e where
    toPaperServerError :: e -> ServerError
    toPaperError :: e -> PaperError
    toPaperError e = PaperError {
        paperServerError = toPaperServerError e
      , paperLogError = e
    }
    toPaperExceptT :: Monad m => e -> PaperExceptT m a
    toPaperExceptT = ExceptT . return . Left . toPaperError

type PaperEither = Either PaperError
type PaperExceptT = ExceptT PaperError

instance {-# OVERLAPPING #-} MonadIO m => MonadIO (PaperExceptT m) where
    liftIO io = ExceptT $ liftIO $ Right <$> io

instance MonadUnliftIO m => MonadUnliftIO (PaperExceptT m) where
    withRunInIO inner = ExceptT $ UnliftIO.Exception.catch (Right <$> withRunInIO (\f -> inner (f . unwrap))) (\(p :: PaperError) ->
        return $ Left $ p
        )
        where
            unwrap :: MonadUnliftIO m => PaperExceptT m a -> m a
            unwrap (ExceptT mpa) = do
                pa <- mpa
                case pa of
                    Right a -> return a
                    Left p -> UnliftIO.Exception.throwIO p

paperLift :: (HasCallStack, MonadUnliftIO m) => m a -> PaperExceptT m a
paperLift ma = ExceptT $ UnliftIO.Exception.catch (Right <$> ma) (\(ex :: SomeException) ->
    return $ Left $ toPaperError $ PaperOuterException ex callStack'
    )

unsafePaperExceptTToSafe :: (HasCallStack, MonadUnliftIO m) => PaperExceptT m a -> PaperExceptT m a
unsafePaperExceptTToSafe (ExceptT mpa) = do
    pa <- paperLift mpa
    case pa of
        Right a -> return a
        Left ex -> ExceptT $ return $ Left ex

paperLiftIO :: (HasCallStack, MonadUnliftIO m) => IO a -> PaperExceptT m a
paperLiftIO = paperLift . liftIO

runPaperEither :: HasCallStack => PaperEither a -> Servant.Handler a
runPaperEither p = case p of
    Left ex -> do
        currentTime <- paperLog getCurrentTime
        let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
        paperLog $ Prelude.putStrLn (paperErrorHeader formattedDate ++ show ex)
        throwError (paperServerError ex)
    Right x -> return x

paperErrorHeader :: String -> String
paperErrorHeader formattedDate = "[PaperError]\t" ++ formattedDate ++ "\n"

runPaperExceptT :: PaperExceptT IO a -> Servant.Handler a
runPaperExceptT p = do
    a' <- liftIO $ (runExceptT p)
    runPaperEither a'

paperLog :: HasCallStack => IO a -> Servant.Handler a
paperLog io = Control.Monad.Catch.catch (liftIO io) (\(ex :: SomeException) -> do
    let ex' = toPaperError $ PaperOuterException ex callStack'
    currentTime <- liftIO getCurrentTime
    let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    liftIO $ Prelude.putStrLn (paperErrorHeader formattedDate ++ show ex')
    throwError $ paperServerError ex'
    )

maybeToPaperEither :: ToPaperError p => Maybe a -> p -> PaperEither a
maybeToPaperEither a' ex = case a' of
        Just a -> Right a
        Nothing -> Left $ toPaperError ex

maybeTToPaperExceptT :: (ToPaperError p, MonadUnliftIO m) => MaybeT IO a -> p -> PaperExceptT m a
maybeTToPaperExceptT (MaybeT ima) ex = do
    a' <- paperLiftIO ima
    case a' of
        Just a -> return a
        Nothing -> toPaperExceptT ex

paperAssert :: (ToPaperError p, Monad m) => Bool -> p -> PaperExceptT m ()
paperAssert b p =
    if b then
        return ()
    else
        toPaperExceptT p

data PaperOuterException where
    PaperOuterException :: Exception e => e -> CallStack -> PaperOuterException

instance Show PaperOuterException where
    show (PaperOuterException ex cs) = "[PaperOuterException]\n" ++ show ex ++ "\n" ++ prettyCallStack cs

instance ToPaperError PaperOuterException where
    toPaperServerError _ = err500 { errBody = "Unexpected Outer Exception" }

data PaperException = PaperException String ServerError CallStack

instance Show PaperException where
    show (PaperException msg _ cs) = "[PaperException]\n" ++ show msg ++ "\n" ++ prettyCallStack cs

instance ToPaperError PaperException where
    toPaperServerError (PaperException _ serverError _) = serverError