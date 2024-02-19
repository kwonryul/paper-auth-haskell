module Exception(
    PaperExcept
  , PaperExceptT
  , maybeToPaperExcept
  , maybeToPaperExceptT
  , maybeToPaperExceptT'
  , liftIOExceptT
  , liftIOExceptT'
  , PaperException(
      IOException
    , ConfigMissing
    , DatabaseInitializeException
  )
  , paperIO
  , paperIO'
  , runPaperExcept
  , runPaperExceptT
  , runPaperExceptM
) where

import Import
import CallStack

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import GHC.Stack

type PaperExcept = Either PaperException
type PaperExceptT = ExceptT PaperException

maybeToPaperExcept :: Maybe a -> PaperException -> PaperExcept a
maybeToPaperExcept a' ex =
    case a' of
        Just a -> Right a
        Nothing -> Left ex

maybeToPaperExceptT :: HasCallStack => MaybeT IO a -> PaperException -> PaperExceptT IO a
maybeToPaperExceptT (MaybeT ima) = maybeToPaperExceptT' ima

maybeToPaperExceptT' :: HasCallStack => IO (Maybe a) -> PaperException -> PaperExceptT IO a
maybeToPaperExceptT' ima ex =
    do
        a' <- paperIO ima
        case a' of
            Just a -> return a
            Nothing -> ExceptT $ return $ Left ex

liftIOExceptT :: MonadIO m => PaperExceptT IO a -> PaperExceptT m a
liftIOExceptT pia = liftIOExceptT' $ runExceptT pia

liftIOExceptT' :: MonadIO m => IO (Either PaperException a) -> PaperExceptT m a
liftIOExceptT' iea = ExceptT $ liftIO iea

data PaperException where
    IOException :: Exception e => e -> CallStack -> PaperException
    ConfigMissing :: String -> CallStack -> PaperException
    DatabaseInitializeException :: (Show db, DB db) => db -> CallStack -> PaperException

instance Show PaperException where
    show (IOException ex cs) = "[IOException]\t" ++ show ex ++ "\n" ++ prettyCallStack cs
    show (ConfigMissing name cs) = "[ConfigMissing]\t" ++ name ++ "\n" ++ prettyCallStack cs
    show (DatabaseInitializeException db cs) = "[DatabaseInitializeException]\t" ++ show db ++ "\n" ++ prettyCallStack cs

instance Exception PaperException

paperIO :: (HasCallStack, MonadIO m) => IO a -> PaperExceptT m a
paperIO io = ExceptT $ liftIO $ catch (Right <$> io) (\(ex :: SomeException) ->
        return $ Left $ IOException ex callStack'
    )

paperIO' :: HasCallStack => IO a -> IO a
paperIO' io = catch io (\(ex :: SomeException) ->
        throwIO (IOException ex callStack')
    )

runPaperExcept :: MonadIO m => PaperExcept a -> m a
runPaperExcept p = case p of
    Left ex -> liftIO $ throwIO ex
    Right x -> return x

runPaperExceptT :: MonadIO m => PaperExceptT m a -> m a
runPaperExceptT (ExceptT mea) = runPaperExceptM mea

runPaperExceptM :: MonadIO m => m (PaperExcept a) -> m a
runPaperExceptM mea = do
    ea <- mea
    runPaperExcept ea