module Verification.Service(
    VerificationServiceI(
        verifyRequest
      , verifyCheck
      )
) where

import qualified Verification.Repository
import Verification.Repository(VerificationRepositoryI)

import Verification.DTO
import Verification.Entity
import Verification.Util
import SMS.ExService
import DB
import PaperMonad

import Servant
import Database.Persist.Sql
import Data.Configurator.Types

import Control.Monad.IO.Unlift
import Data.Time
import GHC.Stack

class (DBI p, VerificationRepositoryI p, VerificationUtilI p, SMSExServiceI p) => VerificationServiceI p where
    verifyRequest :: (HasCallStack, MonadUnliftIO m) => Config -> String -> PaperAuthPool -> PaperMonad p m NoContent
    verifyRequest = verifyRequestImpl
    verifyRequest' :: (HasCallStack, MonadUnliftIO m) => Config -> String -> PaperAuthConn -> PaperMonad p m NoContent
    verifyRequest' = verifyRequest'Impl
    verifyCheck :: (HasCallStack, MonadUnliftIO m) => String -> String -> PaperAuthPool -> PaperMonad p m VerifyCheckResDTO
    verifyCheck = verifyCheckImpl
    verifyCheck' :: (HasCallStack, MonadUnliftIO m) => String -> String -> PaperAuthPool -> PaperAuthConn -> PaperMonad p m VerifyCheckResDTO
    verifyCheck' = verifyCheck'Impl

verifyRequestImpl :: (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Config -> String -> PaperAuthPool -> PaperMonad p m NoContent
verifyRequestImpl cfg phoneNumber pool = runSqlPoolOneConnection (verifyRequest' cfg phoneNumber) pool

verifyRequest'Impl :: (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => Config -> String -> PaperAuthConn -> PaperMonad p m NoContent
verifyRequest'Impl cfg phoneNumber' conn = do
    phoneNumber <- stringToPhoneNumber phoneNumber'
    phoneNumberSecret <- generatePhoneNumberSecret
    iat <- paperLiftIOUnliftIO getCurrentTime
    let expire = addUTCTime (fromInteger 180) iat
    let deleteAt = addUTCTime (fromInteger 1800) iat
    Verification.Repository.deleteByPhoneNumber phoneNumber conn
    _ <- Verification.Repository.newVerification phoneNumber phoneNumberSecret iat expire deleteAt conn
    smsNotify cfg phoneNumber phoneNumberSecret
    return NoContent

verifyCheckImpl :: (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => String -> String -> PaperAuthPool -> PaperMonad p m VerifyCheckResDTO
verifyCheckImpl phoneNumber phoneNumberSecret pool = runSqlPoolOneConnection (verifyCheck' phoneNumber phoneNumberSecret pool) pool

verifyCheck'Impl :: forall p m. (HasCallStack, VerificationServiceI p, MonadUnliftIO m) => String -> String -> PaperAuthPool -> PaperAuthConn -> PaperMonad p m VerifyCheckResDTO
verifyCheck'Impl phoneNumber' phoneNumberSecret pool conn = do
    phoneNumber <- stringToPhoneNumber phoneNumber'
    currentUTC <- paperLiftIOUnliftIO getCurrentTime
    verificationEntity' <- Verification.Repository.findByPhoneNumber phoneNumber conn
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
        inner :: HasCallStack => VerificationId -> Int -> PaperAuthConn -> PaperMonad p m ()
        inner verificationId failCount innerConn =
                if failCount == 4 then
                    Verification.Repository.deleteById verificationId innerConn
                else
                    Verification.Repository.increaseFailCount verificationId innerConn