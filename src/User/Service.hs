{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module User.Service(
    verifyRequest
  , verifyCheck
  , enroll
) where

import qualified User.Repository
import qualified Verification.Repository
import qualified Verification.Service

import User.DTO
import JWT.Util
import JWT.Model
import Verification.Util
import Verification.DTO
import Verification.Entity
import DB
import PaperError
import Import
import CallStack

import Servant
import Database.Persist.Sql
import Database.Persist.Typed
import Web.JWT
import Web.Cookie
import Data.Configurator.Types
import Data.Aeson
import Crypto.BCrypt

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.IO.Unlift
import Data.Set hiding (deleteAt)
import Data.Time
import Data.ByteString.Char8
import GHC.Stack

verifyRequest :: (HasCallStack, MonadUnliftIO m) => PaperAuthPool -> String -> PaperExceptT m NoContent
verifyRequest pool phoneNumber = do
    unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\conn ->
        catchE (inner conn) $ \e -> do
            paperLift $ runReaderT transactionUndo (generalizeSqlBackend conn)
            ExceptT $ return $ Left e
            )) pool
    where
        inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PaperExceptT m NoContent
        inner conn = verifyRequest' conn phoneNumber

verifyRequest' :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> String -> PaperExceptT m NoContent
verifyRequest' conn phoneNumber' = do
    phoneNumber <- stringToPhoneNumber phoneNumber'
    phoneNumberSecret <- generatePhoneNumberSecret
    iat <- paperLiftIO getCurrentTime
    let expire = addUTCTime (fromInteger 180) iat
    let deleteAt = addUTCTime (fromInteger 1800) iat
    Verification.Repository.deleteByPhoneNumber conn phoneNumber
    _ <- Verification.Repository.newVerification conn phoneNumber phoneNumberSecret iat expire deleteAt
    -- send message
    return NoContent

verifyCheck :: (HasCallStack, MonadUnliftIO m) => PaperAuthPool -> String -> String -> PaperExceptT m VerifyCheckResDTO
verifyCheck pool phoneNumber phoneNumberSecret = do
    unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\conn ->
        catchE (inner conn) $ \e -> do
            paperLift $ runReaderT transactionUndo (generalizeSqlBackend conn)
            ExceptT $ return $ Left e
            )) pool
    where
        inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PaperExceptT m VerifyCheckResDTO
        inner conn = verifyCheck' pool conn phoneNumber phoneNumberSecret

verifyCheck' :: (HasCallStack, MonadUnliftIO m) => PaperAuthPool -> PaperAuthConn -> String -> String -> PaperExceptT m VerifyCheckResDTO
verifyCheck' pool conn phoneNumber' phoneNumberSecret = do
    phoneNumber <- stringToPhoneNumber phoneNumber'
    currentUTC <- paperLiftIO getCurrentTime
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
                unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\innerConn ->
                    catchE (inner innerConn verificationId verificationFailCount) $ \e -> do
                        paperLift $ runReaderT transactionUndo (generalizeSqlBackend innerConn)
                        ExceptT $ return $ Left e
                        )) pool
                return $ VerifyCheckResDTO False $ verificationFailCount + 1
            else
                return $ VerifyCheckResDTO True verificationFailCount
        Nothing -> return $ VerifyCheckResDTO False 0
    where
        inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> VerificationId -> Int -> PaperExceptT m ()
        inner innerConn verificationId failCount =
                if failCount == 4 then
                    Verification.Repository.deleteById innerConn verificationId
                else
                    Verification.Repository.increaseFailCount innerConn verificationId

enroll :: (HasCallStack, MonadUnliftIO m) => Config -> PaperAuthPool -> EncodeSigner -> String -> String -> String -> String -> String -> PaperExceptT m (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
enroll config pool encodeSigner paperId password name phoneNumber phoneNumberSecret = do
    unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\conn ->
        catchE (inner conn) $ \e -> do
            paperLift $ runReaderT transactionUndo (generalizeSqlBackend conn)
            ExceptT $ return $ Left e
            )) pool
    where
        inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PaperExceptT m (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
        inner conn = enroll' config pool conn encodeSigner paperId password name phoneNumber phoneNumberSecret

enroll' :: (HasCallStack, MonadUnliftIO m) => Config -> PaperAuthPool -> PaperAuthConn -> EncodeSigner -> String -> String -> String -> String -> String -> PaperExceptT m (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
enroll' config pool conn encodeSigner paperId password name phoneNumber' phoneNumberSecret = do
    phoneNumber <- stringToPhoneNumber phoneNumber'
    currentUTC <- paperLiftIO getCurrentTime
    verificationEntity' <- Verification.Repository.findByPhoneNumber conn phoneNumber
    case verificationEntity' of
        Just (Entity verificationId Verification {
            verificationPhoneNumberSecret
          , verificationFailCount
          }) ->
            if phoneNumberSecret /= verificationPhoneNumberSecret then do
                unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\innerConn ->
                    catchE (inner innerConn verificationId verificationFailCount) $ \e -> do
                        paperLift $ runReaderT transactionUndo (generalizeSqlBackend innerConn)
                        ExceptT $ return $ Left e
                        )) pool
                toPaperExceptT $ PaperException "phoneNumberSecret wrong" (err403 { errBody =
                    encode $ PhoneNumberSecretWrongDTO "phoneNumberSecret wrong" $ verificationFailCount + 1
                  }) callStack'
            else
                return ()
        Nothing -> toPaperExceptT $ PaperException "verification missing" (err403 { errBody = "verification missing" }) callStack'
    sameUserIdEntity' <- User.Repository.findByPaperId conn paperId
    case sameUserIdEntity' of
        Just _ ->
            toPaperExceptT $ PaperException "paperId duplicate" (err400 { errBody = "paperId duplicate" }) callStack'
        Nothing -> return ()
    samePhoneNumberList <- User.Repository.findByPhoneNumber conn phoneNumber
    case samePhoneNumberList of
        [] -> return ()
        _ ->
            toPaperExceptT $ PaperException "phoneNumber duplicate" (err400 { errBody = "phoneNumber duplicate" }) callStack'
    hashedPassword <- maybeTToPaperExceptT
        (MaybeT $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (Data.ByteString.Char8.pack password))
        $ PaperException "hashing string error" (err500 { errBody = "Internal server error" }) callStack'
    userId <- User.Repository.newUser conn Paper paperId hashedPassword name (Just phoneNumber) currentUTC
    let roleSet = Data.Set.empty
        preAuthenticatedUser = PreAuthenticatedUser { userId, roleSet }
    JWTDTO { accessToken, refreshToken } <- Verification.Service.issueJWT config conn encodeSigner preAuthenticatedUser currentUTC
    let cookie = generateRefreshTokenCookie refreshToken
    return $ addHeader cookie $ EnrollResDTO { accessToken }
    where
        inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> VerificationId -> Int -> PaperExceptT m ()
        inner innerConn verificationId failCount =
                if failCount == 4 then
                    Verification.Repository.deleteById innerConn verificationId
                else
                    Verification.Repository.increaseFailCount innerConn verificationId