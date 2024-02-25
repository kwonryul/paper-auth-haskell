{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module JWT.AuthCheck(
    authContext
) where

import DB
import JWT.Model

import Servant
import Servant.Auth.Server

{-}
type AuthContext = Context (
    BasicAuthCheck AuthenticatedUser
    ': '[]
-}

type AuthContext = Context '[]

authContext :: PaperAuthPool -> AuthContext
authContext pool = EmptyContext

{-
basicJWTAuthCheck :: PaperAuthPool -> BasicAuthCheck AuthenticatedUser
basicJWTAuthCheck pool =
    BasicAuthCheck $ basicAuthCheck' pool
    where
        basicAuthCheck' :: PaperAuthPool -> BasicAuthData -> IO (BasicAuthResult AuthenticatedUser)
        basicAuthCheck' _ (BasicAuthData username password) = do
            if username == "hogu" && password == "mogu" then
                return $ Authorized $ AuthenticatedUser 34 []
            else
                return Unauthorized
-}