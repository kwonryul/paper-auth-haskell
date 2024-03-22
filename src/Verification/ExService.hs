{-# LANGUAGE OverloadedStrings #-}

module Verification.ExService(
    VerificationExServiceI(
        verifyVerification
      , verifyPaperId
      , verifyPhoneNumber
      )
) where

import qualified User.Repository
import User.Repository(UserRepositoryI)
import qualified Verification.Repository
import Verification.Repository(VerificationRepositoryI)

import Verification.Entity
import Verification.ExDTO
import Verification.Util
import CallStack
import DB
import PaperMonad

import Servant
import Database.Persist.Sql
import Data.Aeson

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import GHC.Stack

class (DBI p, UserRepositoryI p, VerificationExDTOI p, VerificationRepositoryI p) => VerificationExServiceI p where
    verifyVerification :: (HasCallStack, MonadUnliftIO m) => PhoneNumber -> String -> PaperAuthPool -> PaperAuthConn -> PaperMonad p m ()
    verifyVerification = verifyVerificationImpl
    verifyPaperId :: (HasCallStack, MonadUnliftIO m) => String -> PaperAuthConn -> PaperMonad p m ()
    verifyPaperId = verifyPaperIdImpl
    verifyPhoneNumber :: (HasCallStack, MonadUnliftIO m) => PhoneNumber -> PaperAuthConn -> PaperMonad p m ()
    verifyPhoneNumber = verifyPhoneNumberImpl

verifyVerificationImpl :: forall p m. (HasCallStack, VerificationExServiceI p, MonadUnliftIO m) => PhoneNumber -> String -> PaperAuthPool -> PaperAuthConn -> PaperMonad p m ()
verifyVerificationImpl phoneNumber phoneNumberSecret pool conn = do
    profile <- ask
    verificationEntity' <- Verification.Repository.findByPhoneNumber phoneNumber conn
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
        Nothing -> toPaperMonad $ PaperError "verification missing" (err403 { errBody = "verification missing" }) $ callStack' profile
    where
        inner :: HasCallStack => VerificationId -> Int -> PaperAuthConn -> PaperMonad p m ()
        inner verificationId failCount innerConn =
                if failCount == 4 then
                    Verification.Repository.deleteById verificationId innerConn
                else
                    Verification.Repository.increaseFailCount verificationId innerConn

verifyPaperIdImpl :: (HasCallStack, VerificationExServiceI p, MonadUnliftIO m) => String -> PaperAuthConn -> PaperMonad p m ()
verifyPaperIdImpl paperId conn = do
    profile <- ask
    sameUserIdEntityList <- User.Repository.findByPaperId paperId conn
    case sameUserIdEntityList of
        [] -> return()
        _ ->
            toPaperMonad $ PaperError "paperId duplicate" (err400 { errBody = "paperId duplicate" }) $ callStack' profile

verifyPhoneNumberImpl :: (HasCallStack, VerificationExServiceI p, MonadUnliftIO m) => PhoneNumber -> PaperAuthConn -> PaperMonad p m ()
verifyPhoneNumberImpl phoneNumber conn = do
    profile <- ask
    samePhoneNumberList <- User.Repository.findByPhoneNumber phoneNumber conn
    case samePhoneNumberList of
        [] -> return ()
        _ ->
            toPaperMonad $ PaperError "phoneNumber duplicate" (err400 { errBody = "phoneNumber duplicate" }) (callStack' profile)