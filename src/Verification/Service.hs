{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Verification.Service(
    issueJWT
  , invalidateJWT
) where

import qualified JWT.Repository

import JWT.Model
import JWT.Entity
import User.Entity
import Role.Entity
import DB
import PaperError
import CallStack

import Servant
import Database.Persist.Typed
import Data.Aeson.Types
import Web.JWT
import Data.Configurator
import Data.Configurator.Types

import Control.Monad.IO.Unlift
import Data.Map
import Data.Set
import Data.Vector
import Data.Text
import Data.Int
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Clock
import Data.Traversable
import GHC.Stack

issueJWT :: (HasCallStack, MonadUnliftIO m) => Config -> PaperAuthConn -> EncodeSigner -> PreAuthenticatedUser -> UTCTime -> PaperExceptT m JWTDTO
issueJWT config conn encodeSigner (PreAuthenticatedUser { userId, roleSet }) currentUTC = do
    accessTokenLifetime <- accessTokenLifetime' config
    refreshTokenLifetime <- refreshTokenLifetime' config
    refreshJti <- JWT.Repository.newRefreshToken
        conn userId currentUTC (addUTCTime <$> refreshTokenLifetime <*> Just currentUTC)
    accessJti <- JWT.Repository.newAccessToken
        conn userId currentUTC (addUTCTime <$> accessTokenLifetime <*> Just currentUTC) refreshJti
    accessTokenHeader <- accessTokenHeader' config
    refreshTokenHeader <- refreshTokenHeader' config
    accessTokenClaimsSet <- accessTokenClaimsSet'
        config accessJti currentUTC accessTokenLifetime userId roleSet
    refreshTokenClaimsSet <- refreshTokenClaimsSet'
        config refreshJti currentUTC refreshTokenLifetime userId
    let accessToken = encodeSigned encodeSigner accessTokenHeader accessTokenClaimsSet
        refreshToken = encodeSigned encodeSigner refreshTokenHeader refreshTokenClaimsSet
    JWT.Repository.saveAccessToken conn accessJti accessToken
    JWT.Repository.saveRefreshToken conn refreshJti refreshToken
    return $ JWTDTO accessToken refreshToken

accessTokenHeader' :: (HasCallStack, MonadUnliftIO m) => Config -> PaperExceptT m JOSEHeader
accessTokenHeader' config = do
    typ <- paperLiftIO $ Data.Configurator.lookup config "paper-token.access.header.typ"
    cty <- paperLiftIO $ Data.Configurator.lookup config "paper-token.access.header.cty"
    alg' <- paperLiftIO $ Data.Configurator.lookup @String config "paper-token.access.header.alg"
    let alg = case alg' of
                Just "HS256" -> Just HS256
                Just "RS256" -> Just RS256
                _ -> Nothing
    kid <- paperLiftIO $ Data.Configurator.lookup config "paper-token.access.header.kid"
    return $ JOSEHeader typ cty alg kid

refreshTokenHeader' :: (HasCallStack, MonadUnliftIO m) => Config -> PaperExceptT m JOSEHeader
refreshTokenHeader' config = do
    typ <- paperLiftIO $ Data.Configurator.lookup config "paper-token.refresh.header.typ"
    cty <- paperLiftIO $ Data.Configurator.lookup config "paper-token.refresh.header.cty"
    alg' <- paperLiftIO $ Data.Configurator.lookup @String config "paper-token.refresh.header.alg"
    let alg = case alg' of
                Just "HS256" -> Just HS256
                Just "RS256" -> Just RS256
                _ -> Nothing
    kid <- paperLiftIO $ Data.Configurator.lookup config "paper-token.refresh.header.kid"
    return $ JOSEHeader typ cty alg kid

utcTimeToNominalDiffTime :: UTCTime -> NominalDiffTime
utcTimeToNominalDiffTime utc =
    diffUTCTime utc $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

nominalDiffTimeToNumericDate :: (HasCallStack, MonadUnliftIO m) => Maybe NominalDiffTime -> PaperExceptT m (Maybe NumericDate)
nominalDiffTimeToNumericDate m =
    case m of
        Just n ->
            case numericDate n of
                Just nd -> return $ Just nd
                Nothing -> toPaperExceptT $ PaperException "numericDate invalid" (err500 { errBody = "Internal server error" }) callStack'
        Nothing -> return Nothing

accessTokenLifetime' :: (HasCallStack, MonadUnliftIO m) => Config -> PaperExceptT m (Maybe NominalDiffTime)
accessTokenLifetime' config =
    paperLiftIO $ (fromInteger <$>) <$> Data.Configurator.lookup config "paper-token.access.claims.lifetime"

refreshTokenLifetime' :: (HasCallStack, MonadUnliftIO m) => Config -> PaperExceptT m (Maybe NominalDiffTime)
refreshTokenLifetime' config = 
    paperLiftIO $ (fromInteger <$>) <$> Data.Configurator.lookup config "paper-token.refresh.claims.lifetime"

accessTokenClaimsMap' :: Set Role -> ClaimsMap
accessTokenClaimsMap' roleSet =
    ClaimsMap $ Data.Map.fromList [
        ("roles", Array $ Data.Vector.fromList $ (Data.Aeson.Types.String .pack .roleName) <$> Data.Set.toList roleSet)
    ]

refreshTokenClaimsMap' :: ClaimsMap
refreshTokenClaimsMap' = ClaimsMap $ Data.Map.empty

accessTokenSub' :: UserId -> Maybe StringOrURI
accessTokenSub' userId =
    stringOrURI $ pack $ show $ fromSqlKeyFor userId

refreshTokenSub' :: UserId -> Maybe StringOrURI
refreshTokenSub' userId =
    stringOrURI $ pack $ show $ fromSqlKeyFor userId

stringOrURI' :: (HasCallStack, MonadUnliftIO m) => Maybe Text -> PaperExceptT m (Maybe StringOrURI)
stringOrURI' mt = case mt of
    Just t -> case stringOrURI t of
        Just s -> return $ Just s
        Nothing -> toPaperExceptT $ PaperException "stringOrURI invalid" (err500 { errBody = "Internal server error" }) callStack'
    Nothing -> return Nothing

stringOrURI'' :: (HasCallStack, MonadUnliftIO m) => Int64 -> PaperExceptT m (Maybe StringOrURI)
stringOrURI'' i = do
    case stringOrURI $ pack . show $ i of
        Just s -> return $ Just s
        Nothing -> toPaperExceptT $ PaperException "stringOrURI invalid" (err500 { errBody = "Internal server error" }) callStack'

stringOrURIList :: (HasCallStack, MonadUnliftIO m) => Maybe Text -> PaperExceptT m (Maybe [StringOrURI])
stringOrURIList mt = case mt of
    Just t -> Just <$> stringOrURIList' t
    Nothing -> return Nothing
    where
        stringOrURIList' :: (HasCallStack, MonadUnliftIO m) => Text -> PaperExceptT m [StringOrURI]
        stringOrURIList' t = do
            Data.Traversable.mapM (\t' -> do
                    case stringOrURI t' of
                        Just s -> return s
                        Nothing -> toPaperExceptT $ PaperException "stringOrURI invalid" (err500 { errBody = "Internal server error" }) callStack'
                )
                (Data.Text.words t)

formattedDateToNumericDate :: (HasCallStack, MonadUnliftIO m) => Maybe Text -> PaperExceptT m (Maybe NumericDate)
formattedDateToNumericDate t' = case t' of
    Just t -> Just <$> formattedDateToIntDate' t
    Nothing -> return Nothing
    where
        formattedDateToIntDate' :: (HasCallStack, MonadUnliftIO m) => Text -> PaperExceptT m NumericDate
        formattedDateToIntDate' t = do
            case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (show t) of
                Just utc -> do
                    let nominalDiffTime = diffUTCTime utc $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
                    case numericDate nominalDiffTime of
                        Just n -> return n
                        Nothing -> toPaperExceptT $ PaperException "numericDate invalid" (err500 { errBody = "Internal server error" }) callStack'
                Nothing -> toPaperExceptT $ PaperException "parseTimeM failed" (err500 { errBody = "Internal server error" }) callStack'

accessTokenClaimsSet' :: (HasCallStack, MonadUnliftIO m) => Config -> AccessTokenId -> UTCTime -> Maybe NominalDiffTime -> UserId -> Set Role -> PaperExceptT m JWTClaimsSet
accessTokenClaimsSet' config jti' iat' exp' userId roleSet = do
    iss' <- paperLiftIO $ Data.Configurator.lookup config "paper-token.access.claims.iss"
    iss <- stringOrURI' iss'
    let sub = accessTokenSub' userId
    aud' <- paperLiftIO $ Data.Configurator.lookup config "paper-token.access.claims.aud"
    aud <- (Right <$>) <$> stringOrURIList aud'
    exp'' <- nominalDiffTimeToNumericDate $ (utcTimeToNominalDiffTime iat' +) <$> exp'
    nbf' <- paperLiftIO $ Data.Configurator.lookup config "paper-token.access.claims.nbf"
    nbf <- formattedDateToNumericDate nbf'
    iat <- nominalDiffTimeToNumericDate $ Just $ utcTimeToNominalDiffTime iat'
    jti <- stringOrURI'' $ fromSqlKeyFor jti'
    let unregisteredClaims = accessTokenClaimsMap' roleSet
    return $ JWTClaimsSet iss sub aud exp'' nbf iat jti unregisteredClaims

refreshTokenClaimsSet' :: (HasCallStack, MonadUnliftIO m) => Config -> RefreshTokenId -> UTCTime -> Maybe NominalDiffTime -> UserId -> PaperExceptT m JWTClaimsSet
refreshTokenClaimsSet' config jti' iat' exp' userId = do
    iss' <- paperLiftIO $ Data.Configurator.lookup config "paper-token.refresh.claims.iss"
    iss <- stringOrURI' iss'
    let sub = refreshTokenSub' userId
    aud' <- paperLiftIO $ Data.Configurator.lookup config "paper-token.refresh.claims.aud"
    aud <- (Right <$>) <$> stringOrURIList aud'
    exp'' <- nominalDiffTimeToNumericDate $ (utcTimeToNominalDiffTime iat' +) <$> exp'
    nbf' <- paperLiftIO $ Data.Configurator.lookup config "paper-token.refresh.claims.nbf"
    nbf <- formattedDateToNumericDate nbf'
    iat <- nominalDiffTimeToNumericDate $ Just $ utcTimeToNominalDiffTime iat'
    jti <- stringOrURI'' $ fromSqlKeyFor jti'
    let unregisteredClaims = refreshTokenClaimsMap'
    return $ JWTClaimsSet iss sub aud exp'' nbf iat jti unregisteredClaims

invalidateJWT :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> UserId -> PaperExceptT m ()
invalidateJWT conn userId = do
    JWT.Repository.invalidateJWT conn userId