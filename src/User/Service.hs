{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module User.Service(
    UserServiceI(
        verifyRequest
      , verifyCheck
      , enroll
      )
) where

import qualified User.Repository
import User.Repository(UserRepositoryI)
import qualified Verification.Repository
import Verification.Repository(VerificationRepositoryI)
import qualified Verification.Service
import Verification.Service(VerificationServiceI)

import User.DTO
import JWT.Util
import JWT.Model
import Verification.Util
import Verification.DTO
import Verification.Entity
import DB
import PaperMonad
import Import
import CallStack

import Servant
import Database.Persist.Sql
import Web.JWT
import Web.Cookie
import Data.Configurator.Types
import Data.Aeson
import Crypto.BCrypt

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Unlift
import Data.Set hiding (deleteAt)
import Data.Time
import Data.ByteString.Char8
import GHC.Stack

class (DBI p, JWTUtilI p, UserRepositoryI p, VerificationDTOI p, VerificationRepositoryI p, VerificationServiceI p, VerificationUtilI p) => UserServiceI p where
    verifyRequest :: (HasCallStack, MonadUnliftIO m) => String -> PaperAuthPool -> PaperMonad p m NoContent
    verifyRequest = verifyRequestImpl
    verifyRequest' :: (HasCallStack, MonadUnliftIO m) => String -> PaperAuthConn -> PaperMonad p m NoContent
    verifyRequest' = verifyRequest'Impl
    verifyCheck :: (HasCallStack, MonadUnliftIO m) => String -> String -> PaperAuthPool -> PaperMonad p m VerifyCheckResDTO
    verifyCheck = verifyCheckImpl
    verifyCheck' :: (HasCallStack, MonadUnliftIO m) => String -> String -> PaperAuthPool -> PaperAuthConn -> PaperMonad p m VerifyCheckResDTO
    verifyCheck' = verifyCheck'Impl
    enroll :: (HasCallStack, MonadUnliftIO m) => Config -> EncodeSigner -> String -> String -> String -> String -> String -> PaperAuthPool -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
    enroll = enrollImpl
    enroll' :: (HasCallStack, MonadUnliftIO m) => Config -> EncodeSigner -> String -> String -> String -> String -> String -> PaperAuthPool -> PaperAuthConn -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
    enroll' = enroll'Impl

verifyRequestImpl :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => String -> PaperAuthPool -> PaperMonad p m NoContent
verifyRequestImpl phoneNumber pool = runSqlPoolOneConnection (verifyRequest' phoneNumber) pool

verifyRequest'Impl :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => String -> PaperAuthConn -> PaperMonad p m NoContent
verifyRequest'Impl phoneNumber' conn = do
    phoneNumber <- stringToPhoneNumber phoneNumber'
    phoneNumberSecret <- generatePhoneNumberSecret
    iat <- paperLiftIOUnliftIO getCurrentTime
    let expire = addUTCTime (fromInteger 180) iat
    let deleteAt = addUTCTime (fromInteger 1800) iat
    Verification.Repository.deleteByPhoneNumber conn phoneNumber
    _ <- Verification.Repository.newVerification conn phoneNumber phoneNumberSecret iat expire deleteAt
    -- send message
    return NoContent

verifyCheckImpl :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => String -> String -> PaperAuthPool -> PaperMonad p m VerifyCheckResDTO
verifyCheckImpl phoneNumber phoneNumberSecret pool = runSqlPoolOneConnection (verifyCheck' phoneNumber phoneNumberSecret pool) pool

verifyCheck'Impl :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => String -> String -> PaperAuthPool -> PaperAuthConn -> PaperMonad p m VerifyCheckResDTO
verifyCheck'Impl phoneNumber' phoneNumberSecret pool conn = do
    phoneNumber <- stringToPhoneNumber phoneNumber'
    currentUTC <- paperLiftIOUnliftIO getCurrentTime
    verificationEntity' <- Verification.Repository.findByPhoneNumber conn phoneNumber
    case verificationEntity' of
        Just (Entity verificationId Verification {
            verificationPhoneNumberSecret
          , verificationExpire
          , verificationFailCount
        }) ->
            if diffUTCTime currentUTC verificationExpire > 0 then
                return $ VerifyCheckResDTO False 0
            else if phoneNumberSecret /= verificationPhoneNumberSecret then do
                runSqlPoolOneConnection (inner verificationId verificationFailCount) pool
                return $ VerifyCheckResDTO False $ verificationFailCount + 1
            else
                return $ VerifyCheckResDTO True verificationFailCount
        Nothing -> return $ VerifyCheckResDTO False 0
    where
        inner :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => VerificationId -> Int -> PaperAuthConn -> PaperMonad p m ()
        inner verificationId failCount innerConn =
                if failCount == 4 then
                    Verification.Repository.deleteById innerConn verificationId
                else
                    Verification.Repository.increaseFailCount innerConn verificationId

enrollImpl :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => Config -> EncodeSigner -> String -> String -> String -> String -> String -> PaperAuthPool -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
enrollImpl config encodeSigner paperId password name phoneNumber phoneNumberSecret pool = runSqlPoolOneConnection (enroll' config encodeSigner paperId password name phoneNumber phoneNumberSecret pool) pool

enroll'Impl :: forall p m. (HasCallStack, UserServiceI p, MonadUnliftIO m) => Config -> EncodeSigner -> String -> String -> String -> String -> String -> PaperAuthPool -> PaperAuthConn -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
enroll'Impl config encodeSigner paperId password name phoneNumber' phoneNumberSecret pool conn= do
    profile <- ask
    phoneNumber <- stringToPhoneNumber phoneNumber'
    currentUTC <- paperLiftIOUnliftIO getCurrentTime
    verificationEntity' <- Verification.Repository.findByPhoneNumber conn phoneNumber
    case verificationEntity' of
        Just (Entity verificationId Verification {
            verificationPhoneNumberSecret
          , verificationFailCount
          }) ->
            if phoneNumberSecret /= verificationPhoneNumberSecret then do
                runSqlPoolOneConnection (inner verificationId verificationFailCount) pool
                toPaperMonad $ PaperError "phoneNumberSecret wrong" (err403 { errBody =
                    encode $ phoneNumberSecretWrongDTO profile $ verificationFailCount + 1
                  }) (callStack' profile)
            else
                return ()
        Nothing -> toPaperMonad $ PaperError "verification missing" (err403 { errBody = "verification missing" }) (callStack' profile)
    sameUserIdEntity' <- User.Repository.findByPaperId paperId conn
    case sameUserIdEntity' of
        Just _ ->
            toPaperMonad $ PaperError "paperId duplicate" (err400 { errBody = "paperId duplicate" }) (callStack' profile)
        Nothing -> return ()
    samePhoneNumberList <- User.Repository.findByPhoneNumber phoneNumber conn
    case samePhoneNumberList of
        [] -> return ()
        _ ->
            toPaperMonad $ PaperError "phoneNumber duplicate" (err400 { errBody = "phoneNumber duplicate" }) (callStack' profile)
    hashedPassword <- maybeTToPaperMonadUnliftIO
        (MaybeT $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (Data.ByteString.Char8.pack password))
        $ PaperError "hashing string error" (err500 { errBody = "Internal server error" }) (callStack' profile)
    userId <- User.Repository.newUser Paper paperId hashedPassword name (Just phoneNumber) currentUTC conn
    let roleSet = Data.Set.empty
        preAuthenticatedUser = PreAuthenticatedUser { userId, roleSet }
    JWTDTO { accessToken, refreshToken } <- Verification.Service.issueJWT config conn encodeSigner preAuthenticatedUser currentUTC
    let cookie = generateRefreshTokenCookie (Proxy :: Proxy p) refreshToken
    return $ addHeader cookie $ EnrollResDTO { accessToken }
    where
        inner :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => VerificationId -> Int -> PaperAuthConn -> PaperMonad p m ()
        inner verificationId failCount innerConn =
                if failCount == 4 then
                    Verification.Repository.deleteById innerConn verificationId
                else
                    Verification.Repository.increaseFailCount innerConn verificationId