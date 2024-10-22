{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module JWT.ExService(
    JWTExServiceI(
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

class JWTRepositoryI p => JWTExServiceI p where
    issueJWT :: (HasCallStack, MonadUnliftIO m) => Config -> EncodeSigner -> PreAuthenticatedUser -> UTCTime -> PaperAuthConn -> PaperMonad p m JWTDTO
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
    stringOrURI' :: (HasCallStack, Monad m) => Maybe Text -> PaperMonad p m (Maybe StringOrURI)
    stringOrURI' = stringOrURI'Impl
    stringOrURI'' :: (HasCallStack, Monad m) => Int64 -> PaperMonad p m (Maybe StringOrURI)
    stringOrURI'' = stringOrURI''Impl
    stringOrURIList :: (HasCallStack, Monad m) => Maybe Text -> PaperMonad p m (Maybe [StringOrURI])
    stringOrURIList = stringOrURIListImpl
    formattedDateToNumericDate :: (HasCallStack, Monad m) => Maybe Text -> PaperMonad p m (Maybe NumericDate)
    formattedDateToNumericDate = formattedDateToNumericDateImpl
    accessTokenClaimsSet' :: (HasCallStack, MonadUnliftIO m) => Config -> AccessTokenId -> UTCTime -> Maybe NominalDiffTime -> UserId -> Set Role -> PaperMonad p m JWTClaimsSet
    accessTokenClaimsSet' = accessTokenClaimsSet'Impl
    refreshTokenClaimsSet' :: (HasCallStack, MonadUnliftIO m) => Config -> RefreshTokenId -> UTCTime -> Maybe NominalDiffTime -> UserId -> PaperMonad p m JWTClaimsSet
    refreshTokenClaimsSet' = refreshTokenClaimsSet'Impl
    invalidateJWT :: (HasCallStack, MonadUnliftIO m) => UserId -> PaperAuthConn -> PaperMonad p m ()
    invalidateJWT = invalidateJWTImpl

issueJWTImpl :: (HasCallStack, JWTExServiceI p, MonadUnliftIO m) => Config -> EncodeSigner -> PreAuthenticatedUser -> UTCTime -> PaperAuthConn -> PaperMonad p m JWTDTO
issueJWTImpl config encodeSigner (PreAuthenticatedUser { userId, roleSet }) currentUTC conn = do
    accessTokenLifetime <- accessTokenLifetime' config
    refreshTokenLifetime <- refreshTokenLifetime' config
    refreshJti <- JWT.Repository.newRefreshToken
        userId currentUTC (addUTCTime <$> refreshTokenLifetime <*> Just currentUTC) conn
    accessJti <- JWT.Repository.newAccessToken
        userId currentUTC (addUTCTime <$> accessTokenLifetime <*> Just currentUTC) refreshJti conn
    accessTokenHeader <- accessTokenHeader' config
    refreshTokenHeader <- refreshTokenHeader' config
    accessTokenClaimsSet <- accessTokenClaimsSet'
        config accessJti currentUTC accessTokenLifetime userId roleSet
    refreshTokenClaimsSet <- refreshTokenClaimsSet'
        config refreshJti currentUTC refreshTokenLifetime userId
    let accessToken = encodeSigned encodeSigner accessTokenHeader accessTokenClaimsSet
        refreshToken = encodeSigned encodeSigner refreshTokenHeader refreshTokenClaimsSet
    JWT.Repository.saveAccessToken accessJti accessToken conn
    JWT.Repository.saveRefreshToken refreshJti refreshToken conn
    return $ JWTDTO accessJti accessToken refreshJti refreshToken

accessTokenHeader'Impl :: (HasCallStack, JWTExServiceI p, MonadUnliftIO m) => Config -> PaperMonad p m JOSEHeader
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

refreshTokenHeader'Impl :: (HasCallStack, JWTExServiceI p, MonadUnliftIO m) => Config -> PaperMonad p m JOSEHeader
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

utcTimeToNominalDiffTimeImpl :: JWTExServiceI p => Proxy p -> UTCTime -> NominalDiffTime
utcTimeToNominalDiffTimeImpl _ utc =
    diffUTCTime utc $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

nominalDiffTimeToNumericDateImpl :: (HasCallStack, JWTExServiceI p, MonadUnliftIO m) => Maybe NominalDiffTime -> PaperMonad p m (Maybe NumericDate)
nominalDiffTimeToNumericDateImpl m = do
    profile <- ask
    case m of
        Just n ->
            case numericDate n of
                Just nd -> return $ Just nd
                Nothing -> toPaperMonad $ PaperError "numericDate invalid" (err500 { errBody = "internal server error" }) (callStack' profile)
        Nothing -> return Nothing

accessTokenLifetime'Impl :: (HasCallStack, JWTExServiceI p, MonadUnliftIO m) => Config -> PaperMonad p m (Maybe NominalDiffTime)
accessTokenLifetime'Impl config =
    paperLiftIOUnliftIO $ (fromInteger <$>) <$> Data.Configurator.lookup config "paper-token.access.claims.lifetime"

refreshTokenLifetime'Impl :: (HasCallStack, JWTExServiceI p, MonadUnliftIO m) => Config -> PaperMonad p m (Maybe NominalDiffTime)
refreshTokenLifetime'Impl config = 
    paperLiftIOUnliftIO $ (fromInteger <$>) <$> Data.Configurator.lookup config "paper-token.refresh.claims.lifetime"

accessTokenClaimsMap'Impl :: JWTExServiceI p => Proxy p -> Set Role -> ClaimsMap
accessTokenClaimsMap'Impl _ roleSet =
    ClaimsMap $ Data.Map.fromList [
        ("roles", Array $ Data.Vector.fromList $ (Data.Aeson.Types.String .pack .roleName) <$> Data.Set.toList roleSet)
    ]

refreshTokenClaimsMap'Impl :: JWTExServiceI p => Proxy p -> ClaimsMap
refreshTokenClaimsMap'Impl _ = ClaimsMap $ Data.Map.empty

accessTokenSub'Impl :: JWTExServiceI p => Proxy p -> UserId -> Maybe StringOrURI
accessTokenSub'Impl _ userId =
    stringOrURI $ pack $ show $ fromSqlKeyFor userId

refreshTokenSub'Impl :: JWTExServiceI p => Proxy p -> UserId -> Maybe StringOrURI
refreshTokenSub'Impl _ userId =
    stringOrURI $ pack $ show $ fromSqlKeyFor userId

stringOrURI'Impl :: (HasCallStack, JWTExServiceI p, Monad m) => Maybe Text -> PaperMonad p m (Maybe StringOrURI)
stringOrURI'Impl mt = do
    profile <- ask
    case mt of
        Just t -> case stringOrURI t of
            Just s -> return $ Just s
            Nothing -> toPaperMonad $ PaperError "stringOrURI invalid" (err500 { errBody = "internal server error" }) (callStack' profile)
        Nothing -> return Nothing

stringOrURI''Impl :: (HasCallStack, JWTExServiceI p, Monad m) => Int64 -> PaperMonad p m (Maybe StringOrURI)
stringOrURI''Impl i = do
    profile <- ask
    case stringOrURI $ pack . show $ i of
        Just s -> return $ Just s
        Nothing -> toPaperMonad $ PaperError "stringOrURI invalid" (err500 { errBody = "internal server error" }) (callStack' profile)

stringOrURIListImpl :: forall p m. (HasCallStack, JWTExServiceI p, Monad m) => Maybe Text -> PaperMonad p m (Maybe [StringOrURI])
stringOrURIListImpl mt = do
    profile <- ask
    case mt of
        Just t -> Just <$> stringOrURIList' profile t
        Nothing -> return Nothing
    where
        stringOrURIList' :: HasCallStack => Proxy p -> Text -> PaperMonad p m [StringOrURI]
        stringOrURIList' profile t = do
            Data.Traversable.mapM (\t' -> do
                    case stringOrURI t' of
                        Just s -> return s
                        Nothing -> toPaperMonad $ PaperError "stringOrURI invalid" (err500 { errBody = "internal server error" }) (callStack' profile)
                )
                (Data.Text.words t)

formattedDateToNumericDateImpl :: forall p m. (HasCallStack, JWTExServiceI p, Monad m) => Maybe Text -> PaperMonad p m (Maybe NumericDate)
formattedDateToNumericDateImpl t' = do
    profile <- ask
    case t' of
        Just t -> Just <$> formattedDateToIntDate' profile t
        Nothing -> return Nothing
    where
        formattedDateToIntDate' :: HasCallStack => Proxy p -> Text -> PaperMonad p m NumericDate
        formattedDateToIntDate' profile t = do
            case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (show t) of
                Just utc -> do
                    let nominalDiffTime = diffUTCTime utc $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
                    case numericDate nominalDiffTime of
                        Just n -> return n
                        Nothing -> toPaperMonad $ PaperError "numericDate invalid" (err500 { errBody = "internal server error" }) (callStack' profile)
                Nothing -> toPaperMonad $ PaperError "parseTimeM failed" (err500 { errBody = "internal server error" }) (callStack' profile)

accessTokenClaimsSet'Impl :: (HasCallStack, JWTExServiceI p, MonadUnliftIO m) => Config -> AccessTokenId -> UTCTime -> Maybe NominalDiffTime -> UserId -> Set Role -> PaperMonad p m JWTClaimsSet
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

refreshTokenClaimsSet'Impl :: (HasCallStack, JWTExServiceI p, MonadUnliftIO m) => Config -> RefreshTokenId -> UTCTime -> Maybe NominalDiffTime -> UserId -> PaperMonad p m JWTClaimsSet
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

invalidateJWTImpl :: (HasCallStack, JWTExServiceI p, MonadUnliftIO m) => UserId -> PaperAuthConn -> PaperMonad p m ()
invalidateJWTImpl userId conn = do
    JWT.Repository.invalidateJWT userId conn