{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module User.Service(
    verifyRequest
  , verifyCheck
  , enroll
) where

import qualified User.Repository
import qualified Verification.Repository
import qualified Verification.Service

import User.DTO
import JWT.Model
import Verification.Util
import Verification.Entity
import DB
import PaperError
import CallStack

import Servant
import Database.Persist.Sql
import Database.Persist.Typed
import Web.JWT
import Data.Configurator.Types
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

verifyCheck :: (HasCallStack, MonadUnliftIO m) => PaperAuthPool -> String -> String -> PaperExceptT m Bool
verifyCheck pool phoneNumber phoneNumberSecret = do
    unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\conn ->
        catchE (inner conn) $ \e -> do
            paperLift $ runReaderT transactionUndo (generalizeSqlBackend conn)
            ExceptT $ return $ Left e
            )) pool
    where
        inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PaperExceptT m Bool
        inner conn = verifyCheck' conn phoneNumber phoneNumberSecret

verifyCheck' :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> String -> String -> PaperExceptT m Bool
verifyCheck' conn phoneNumber' phoneNumberSecret = do
    phoneNumber <- stringToPhoneNumber phoneNumber'
    currentUTC <- paperLiftIO getCurrentTime
    verificationEntityList <- Verification.Repository.findByPhoneNumber conn phoneNumber
    case verificationEntityList of
        [] -> return False
        (Entity _ Verification {
            verificationPhoneNumberSecret
          , verificationExpire
          } : []) ->
            if diffUTCTime currentUTC verificationExpire > 0 then
                return False
            else if phoneNumberSecret /= verificationPhoneNumberSecret then
                return False
            else
                return True
        (_ : _ : _) -> do
            Verification.Repository.deleteByPhoneNumber conn phoneNumber
            toPaperExceptT $ PaperException "duplicate phoneNumber in verification" (err500 { errBody = "database error" }) callStack'

enroll :: (HasCallStack, MonadUnliftIO m) => Config -> PaperAuthPool -> EncodeSigner -> String -> String -> String -> String -> String -> PaperExceptT m EnrollResDTO
enroll config pool encodeSigner paperId password name phoneNumber phoneNumberSecret = do
    unsafePaperExceptTToSafe $ runSqlPoolFor (ReaderT (\conn ->
        catchE (inner conn) $ \e -> do
            paperLift $ runReaderT transactionUndo (generalizeSqlBackend conn)
            ExceptT $ return $ Left e
            )) pool
    where
        inner :: (HasCallStack, MonadUnliftIO m) => PaperAuthConn -> PaperExceptT m EnrollResDTO
        inner conn = enroll' config conn encodeSigner paperId password name phoneNumber phoneNumberSecret

enroll' :: (HasCallStack, MonadUnliftIO m) => Config -> PaperAuthConn -> EncodeSigner -> String -> String -> String -> String -> String -> PaperExceptT m EnrollResDTO
enroll' config conn encodeSigner paperId password name phoneNumber' phoneNumberSecret = do
    phoneNumber <- stringToPhoneNumber phoneNumber'
    currentUTC <- paperLiftIO getCurrentTime
    verificationEntityList <- Verification.Repository.findByPhoneNumber conn phoneNumber
    case verificationEntityList of
        [] -> toPaperExceptT $ PaperException "verification missing" (err403 { errBody = "verification missing" }) callStack'
        (Entity _ Verification {
            verificationPhoneNumberSecret
          } : []) ->
            if phoneNumberSecret /= verificationPhoneNumberSecret then
                toPaperExceptT $ PaperException "phoneNumberSecret wrong" (err403 { errBody = "phoneNumberSecret wrong" }) callStack'
            else
                return ()
        (_ : _ : _) -> do
            Verification.Repository.deleteByPhoneNumber conn phoneNumber
            toPaperExceptT $ PaperException "duplicate phoneNumber in verification" (err500 { errBody = "database error" }) callStack'
    sameUserIdList <- User.Repository.findByPaperId conn paperId
    case sameUserIdList of
        [] -> return ()
        _ ->
            toPaperExceptT $ PaperException "paperId duplicate" (err400 { errBody = "paperId duplicate" }) callStack'
    samePhoneNumberList <- User.Repository.findByPhoneNumber conn phoneNumber
    case samePhoneNumberList of
        [] -> return ()
        _ ->
            toPaperExceptT $ PaperException "phoneNumber duplicate" (err400 { errBody = "phoneNumber duplicate" }) callStack'
    hashedPassword <- maybeTToPaperExceptT
        (MaybeT $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (Data.ByteString.Char8.pack password))
        $ PaperException "hashing string error" (err500 { errBody = "Internal server error" }) callStack'
    userId <- User.Repository.newUser conn TypePaper paperId hashedPassword name (Just phoneNumber) currentUTC
    let roleSet = Data.Set.empty
        authenticatedUser = AuthenticatedUser { userId, roleSet }
    jwtDTO <- Verification.Service.issueJWT config conn encodeSigner authenticatedUser currentUTC
    return $ fromJWTDTO jwtDTO