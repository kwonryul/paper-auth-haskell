{-# LANGUAGE DerivingStrategies #-}

module Monad.ProfileT(
    Profile
  , ProfileT(
        ProfileT
      , unProfileT
      )
) where

import Control.Monad.Reader
import Data.Proxy
import Data.Kind

class Profile p

type ProfileT :: Type -> (Type -> Type) -> Type -> Type
data ProfileT p m a where
    ProfileT :: Profile p => {
        unProfileT :: ReaderT (Proxy p) m a
     } -> ProfileT p m a


instance (Profile p, Functor m) => Functor (ProfileT p m) where
    fmap f (ProfileT x) = ProfileT $ fmap f x

instance (Profile p, Applicative m) => Applicative (ProfileT p m) where
    pure x = ProfileT $ pure x
    ProfileT f <*> ProfileT x = ProfileT $ f <*> x

instance (Profile p, Monad m) => Monad (ProfileT p m) where
    return = pure
    ProfileT x >>= f = ProfileT $ x >>= unProfileT . f

instance Profile p => MonadTrans (ProfileT p) where
    lift = ProfileT . lift

instance (Profile p, Monad m) => MonadReader (Proxy p) (ProfileT p m) where
    ask = ProfileT $ ask
    local f x = ProfileT $ local f $ unProfileT x

instance (Profile p, MonadIO m) => MonadIO (ProfileT p m) where
    liftIO = ProfileT . liftIO