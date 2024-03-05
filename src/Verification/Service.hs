{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Verification.Service(
    VerificationServiceI(
        issueJWT
      , invalidateJWT
      )
) where

import qualified JWT.Repository
import JWT.Repository(JWTRepositoryI)

import JWT.Model
import JWT.Entity
import User.Entity
import Role.Entity
import DB
import PaperMonad
import CallStack

import Servant
import Database.Persist.Typed
import Data.Aeson.Types
import Web.JWT
import Data.Configurator
import Data.Configurator.Types

import Control.Monad.IO.Unlift
import Control.Monad.Reader
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

class JWTRepositoryI p => VerificationServiceI p where
    issueJWT :: (HasCallStack, MonadUnliftIO m) => Config -> PaperAuthConn -> EncodeSigner -> PreAuthenticatedUser -> UTCTime -> PaperMonad p m JWTDTO
    issueJWT = issueJWTImpl
    accessTokenHeader' :: (HasCallStack, MonadUnliftIO m) => Config -> PaperMonad p m JOSEHeader
    accessTokenHeader' = accessTokenHeader'Impl
    refreshTokenHeader' :: (HasCallStack, MonadUnliftIO m) => Config -> PaperMonad p m JOSEHeader
    refreshTokenHeader' = refreshTokenHeader'Impl
    utcTimeToNominalDiffTime :: Proxy p -> UTCTime -> NominalDiffTime
    utcTimeToNominalDiffTime = utcTimeToNominalDiffTimeImpl
    nominalDiffTimeToNumericDate :: (HasCallStack, MonadUnliftIO m) => Maybe NominalDiffTime -> PaperMonad p m (Maybe NumericDate)
    nominalDiffTimeToNumericDate = nominalDiffTimeToNumericDateImpl
    accessTokenLifetime' :: (HasCallStack, MonadUnliftIO m) => Config -> PaperMonad p m (Maybe NominalDiffTime)
    accessTokenLifetime' = accessTokenLifetime'Impl
    refreshTokenLifetime' :: (HasCallStack, MonadUnliftIO m) => Config -> PaperMonad p m (Maybe NominalDiffTime)
    refreshTokenLifetime' = refreshTokenLifetime'Impl
    accessTokenClaimsMap' :: Proxy p -> Set Role -> ClaimsMap
    accessTokenClaimsMap' = accessTokenClaimsMap'Impl
    refreshTokenClaimsMap' :: Proxy p -> ClaimsMap
    refreshTokenClaimsMap' = refreshTokenClaimsMap'Impl
    accessTokenSub' :: Proxy p -> UserId -> Maybe StringOrURI
    accessTokenSub' = accessTokenSub'Impl
    refreshTokenSub' :: Proxy p -> UserId -> Maybe StringOrURI
    refreshTokenSub' = refreshTokenSub'Impl
    stringOrURI' :: (HasCallStack, MonadUnliftIO m) => Maybe Text -> PaperMonad p m (Maybe StringOrURI)
    stringOrURI' = stringOrURI'Impl
    stringOrURI'' :: (HasCallStack, MonadUnliftIO m) => Int64 -> PaperMonad p m (Maybe StringOrURI)
    stringOrURI'' = stringOrURI''Impl
    stringOrURIList :: (HasCallStack, MonadUnliftIO m) => Maybe Text -> PaperMonad p m (Maybe [StringOrURI])
    stringOrURIList = stringOrURIListImpl
    formattedDateToNumericDate :: (HasCallStack, MonadUnliftIO m) => Maybe Text -> PaperMonad p m (Maybe NumericDate)
    formattedDateToNumericDate = formattedDateToNumericDateImpl
    accessTokenClaimsSet' :: (HasCallStack, MonadUnliftIO m) => Config -> AccessTokenId -> UTCTime -> Maybe NominalDiffTime -> UserId -> Set Role -> PaperMonad p m JWTClaimsSet
    accessTokenClaimsSet' = accessTokenClaimsSet'Impl
    refreshTokenClaimsSet' :: (HasCallStack, MonadUnliftIO m) => Config -> RefreshTokenId -> UTCTime -> Maybe NominalDiffTime -> UserId -> PaperMonad p m JWTClaimsSet
    refreshTokenClaimsSet' = refreshTokenClaimsSet'Impl
    invalidateJWT :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> UserId -> PaperMonad p m ()
    invalidateJWT = invalidateJWTImpl

issueJWTImpl :: (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Config -> PaperAuthConn -> EncodeSigner -> PreAuthenticatedUser -> UTCTime -> PaperMonad p m JWTDTO
issueJWTImpl config conn encodeSigner (PreAuthenticatedUser { userId, roleSet }) currentUTC = do
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
    return $ JWTDTO accessJti accessToken refreshJti refreshToken

accessTokenHeader'Impl :: (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Config -> PaperMonad p m JOSEHeader
accessTokenHeader'Impl config = do
    typ <- paperLiftIOUnliftIO $ Data.Configurator.lookup config "paper-token.access.header.typ"
    cty <- paperLiftIOUnliftIO $ Data.Configurator.lookup config "paper-token.access.header.cty"
    alg' <- paperLiftIOUnliftIO $ Data.Configurator.lookup @String config "paper-token.access.header.alg"
    let alg = case alg' of
                Just "HS256" -> Just HS256
                Just "RS256" -> Just RS256
                _ -> Nothing
    kid <- paperLiftIOUnliftIO $ Data.Configurator.lookup config "paper-token.access.header.kid"
    return $ JOSEHeader typ cty alg kid

refreshTokenHeader'Impl :: (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Config -> PaperMonad p m JOSEHeader
refreshTokenHeader'Impl config = do
    typ <- paperLiftIOUnliftIO $ Data.Configurator.lookup config "paper-token.refresh.header.typ"
    cty <- paperLiftIOUnliftIO $ Data.Configurator.lookup config "paper-token.refresh.header.cty"
    alg' <- paperLiftIOUnliftIO $ Data.Configurator.lookup @String config "paper-token.refresh.header.alg"
    let alg = case alg' of
                Just "HS256" -> Just HS256
                Just "RS256" -> Just RS256
                _ -> Nothing
    kid <- paperLiftIOUnliftIO $ Data.Configurator.lookup config "paper-token.refresh.header.kid"
    return $ JOSEHeader typ cty alg kid

utcTimeToNominalDiffTimeImpl :: VerificationServiceI p => Proxy p -> UTCTime -> NominalDiffTime
utcTimeToNominalDiffTimeImpl _ utc =
    diffUTCTime utc $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

nominalDiffTimeToNumericDateImpl :: forall p m. (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Maybe NominalDiffTime -> PaperMonad p m (Maybe NumericDate)
nominalDiffTimeToNumericDateImpl m =
    case m of
        Just n ->
            case numericDate n of
                Just nd -> return $ Just nd
                Nothing -> toPaperMonad $ PaperError "numericDate invalid" (err500 { errBody = "Internal server error" }) (callStack' profile)
        Nothing -> return Nothing
    where
        profile :: Proxy p
        profile = Proxy

accessTokenLifetime'Impl :: (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Config -> PaperMonad p m (Maybe NominalDiffTime)
accessTokenLifetime'Impl config =
    paperLiftIOUnliftIO $ (fromInteger <$>) <$> Data.Configurator.lookup config "paper-token.access.claims.lifetime"

refreshTokenLifetime'Impl :: (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Config -> PaperMonad p m (Maybe NominalDiffTime)
refreshTokenLifetime'Impl config = 
    paperLiftIOUnliftIO $ (fromInteger <$>) <$> Data.Configurator.lookup config "paper-token.refresh.claims.lifetime"

accessTokenClaimsMap'Impl :: VerificationServiceI p => Proxy p -> Set Role -> ClaimsMap
accessTokenClaimsMap'Impl _ roleSet =
    ClaimsMap $ Data.Map.fromList [
        ("roles", Array $ Data.Vector.fromList $ (Data.Aeson.Types.String .pack .roleName) <$> Data.Set.toList roleSet)
    ]

refreshTokenClaimsMap'Impl :: VerificationServiceI p => Proxy p -> ClaimsMap
refreshTokenClaimsMap'Impl _ = ClaimsMap $ Data.Map.empty

accessTokenSub'Impl :: VerificationServiceI p => Proxy p -> UserId -> Maybe StringOrURI
accessTokenSub'Impl _ userId =
    stringOrURI $ pack $ show $ fromSqlKeyFor userId

refreshTokenSub'Impl :: VerificationServiceI p => Proxy p -> UserId -> Maybe StringOrURI
refreshTokenSub'Impl _ userId =
    stringOrURI $ pack $ show $ fromSqlKeyFor userId

stringOrURI'Impl :: forall p m. (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Maybe Text -> PaperMonad p m (Maybe StringOrURI)
stringOrURI'Impl mt = case mt of
    Just t -> case stringOrURI t of
        Just s -> return $ Just s
        Nothing -> toPaperMonad $ PaperError "stringOrURI invalid" (err500 { errBody = "Internal server error" }) (callStack' profile)
    Nothing -> return Nothing
    where
        profile :: Proxy p
        profile = Proxy

stringOrURI''Impl :: forall p m. (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Int64 -> PaperMonad p m (Maybe StringOrURI)
stringOrURI''Impl i = do
    case stringOrURI $ pack . show $ i of
        Just s -> return $ Just s
        Nothing -> toPaperMonad $ PaperError "stringOrURI invalid" (err500 { errBody = "Internal server error" }) (callStack' profile)
    where
        profile :: Proxy p
        profile = Proxy

stringOrURIListImpl :: forall p m. (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Maybe Text -> PaperMonad p m (Maybe [StringOrURI])
stringOrURIListImpl mt = case mt of
    Just t -> Just <$> stringOrURIList' t
    Nothing -> return Nothing
    where
        stringOrURIList' :: (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Text -> PaperMonad p m [StringOrURI]
        stringOrURIList' t = do
            Data.Traversable.mapM (\t' -> do
                    case stringOrURI t' of
                        Just s -> return s
                        Nothing -> toPaperMonad $ PaperError "stringOrURI invalid" (err500 { errBody = "Internal server error" }) (callStack' profile)
                )
                (Data.Text.words t)
        profile :: Proxy p
        profile = Proxy

formattedDateToNumericDateImpl :: forall p m. (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Maybe Text -> PaperMonad p m (Maybe NumericDate)
formattedDateToNumericDateImpl t' = case t' of
    Just t -> Just <$> formattedDateToIntDate' t
    Nothing -> return Nothing
    where
        formattedDateToIntDate' :: (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Text -> PaperMonad p m NumericDate
        formattedDateToIntDate' t = do
            case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (show t) of
                Just utc -> do
                    let nominalDiffTime = diffUTCTime utc $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
                    case numericDate nominalDiffTime of
                        Just n -> return n
                        Nothing -> toPaperMonad $ PaperError "numericDate invalid" (err500 { errBody = "Internal server error" }) (callStack' profile)
                Nothing -> toPaperMonad $ PaperError "parseTimeM failed" (err500 { errBody = "Internal server error" }) (callStack' profile)
        profile :: Proxy p
        profile = Proxy


accessTokenClaimsSet'Impl :: (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Config -> AccessTokenId -> UTCTime -> Maybe NominalDiffTime -> UserId -> Set Role -> PaperMonad p m JWTClaimsSet
accessTokenClaimsSet'Impl config jti' iat' exp' userId roleSet = do
    proxy <- ask
    iss' <- paperLiftIOUnliftIO $ Data.Configurator.lookup config "paper-token.access.claims.iss"
    iss <- stringOrURI' iss'
    let sub = accessTokenSub' proxy userId
    aud' <- paperLiftIOUnliftIO $ Data.Configurator.lookup config "paper-token.access.claims.aud"
    aud <- (Right <$>) <$> stringOrURIList aud'
    exp'' <- nominalDiffTimeToNumericDate $ (utcTimeToNominalDiffTime proxy iat' +) <$> exp'
    nbf' <- paperLiftIOUnliftIO $ Data.Configurator.lookup config "paper-token.access.claims.nbf"
    nbf <- formattedDateToNumericDate nbf'
    iat <- nominalDiffTimeToNumericDate $ Just $ utcTimeToNominalDiffTime proxy iat'
    jti <- stringOrURI'' $ fromSqlKeyFor jti'
    let unregisteredClaims = accessTokenClaimsMap' proxy roleSet
    return $ JWTClaimsSet iss sub aud exp'' nbf iat jti unregisteredClaims

refreshTokenClaimsSet'Impl :: (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Config -> RefreshTokenId -> UTCTime -> Maybe NominalDiffTime -> UserId -> PaperMonad p m JWTClaimsSet
refreshTokenClaimsSet'Impl config jti' iat' exp' userId = do
    proxy <- ask
    iss' <- paperLiftIOUnliftIO $ Data.Configurator.lookup config "paper-token.refresh.claims.iss"
    iss <- stringOrURI' iss'
    let sub = refreshTokenSub' proxy userId
    aud' <- paperLiftIOUnliftIO $ Data.Configurator.lookup config "paper-token.refresh.claims.aud"
    aud <- (Right <$>) <$> stringOrURIList aud'
    exp'' <- nominalDiffTimeToNumericDate $ (utcTimeToNominalDiffTime proxy iat' +) <$> exp'
    nbf' <- paperLiftIOUnliftIO $ Data.Configurator.lookup config "paper-token.refresh.claims.nbf"
    nbf <- formattedDateToNumericDate nbf'
    iat <- nominalDiffTimeToNumericDate $ Just $ utcTimeToNominalDiffTime proxy iat'
    jti <- stringOrURI'' $ fromSqlKeyFor jti'
    let unregisteredClaims = refreshTokenClaimsMap' proxy
    return $ JWTClaimsSet iss sub aud exp'' nbf iat jti unregisteredClaims

invalidateJWTImpl :: (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => PaperAuthConn -> UserId -> PaperMonad p m ()
invalidateJWTImpl conn userId = do
    JWT.Repository.invalidateJWT conn userId