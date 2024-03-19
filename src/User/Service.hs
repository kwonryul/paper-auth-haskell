{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module User.Service(
    UserServiceI(
        getUserInfo
      , patchUserInfo
      , enroll
      , enroll'
      )
) where

import qualified User.Repository
import User.Repository(UserRepositoryI)
import qualified Verification.Repository
import Verification.Repository(VerificationRepositoryI)
import qualified JWT.ExService
import JWT.ExService(JWTExServiceI)

import JWT.Util
import JWT.Model
import User.DTO
import User.Entity
import Verification.Util
import Verification.ExDTO
import Verification.Entity
import DB
import Enum
import PaperMonad
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

class (DBI p, JWTUtilI p, UserRepositoryI p, VerificationExDTOI p, VerificationRepositoryI p, JWTExServiceI p, VerificationUtilI p) => UserServiceI p where
    getUserInfo :: (HasCallStack, MonadUnliftIO m) => UserId -> PaperAuthPool -> PaperMonad p m GetUserInfoResDTO
    getUserInfo = getUserInfoImpl
    getUserInfo' :: (HasCallStack, MonadUnliftIO m) => UserId -> PaperAuthConn -> PaperMonad p m GetUserInfoResDTO
    getUserInfo' = getUserInfo'Impl
    patchUserInfo :: (HasCallStack, MonadUnliftIO m) => UserId -> Maybe String -> Maybe String -> PaperAuthPool -> PaperMonad p m NoContent
    patchUserInfo = patchUserInfoImpl
    patchUserInfo' :: (HasCallStack, MonadUnliftIO m) => UserId -> Maybe String -> Maybe String -> PaperAuthConn -> PaperMonad p m NoContent
    patchUserInfo' = patchUserInfo'Impl
    enroll :: (HasCallStack, MonadUnliftIO m) => Config -> EncodeSigner -> String -> String -> String -> String -> PaperAuthPool -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
    enroll = enrollImpl
    enroll' :: (HasCallStack, MonadUnliftIO m) => Config -> EncodeSigner -> String -> String ->  String -> String -> PaperAuthPool -> PaperAuthConn -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
    enroll' = enroll'Impl

getUserInfoImpl :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => UserId -> PaperAuthPool -> PaperMonad p m GetUserInfoResDTO
getUserInfoImpl userId pool =
    runSqlPoolOneConnection (getUserInfo' userId) pool

getUserInfo'Impl :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => UserId -> PaperAuthConn -> PaperMonad p m GetUserInfoResDTO
getUserInfo'Impl userId conn =
    User.Repository.getUserInfo userId conn

patchUserInfoImpl :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => UserId -> Maybe String -> Maybe String -> PaperAuthPool -> PaperMonad p m NoContent
patchUserInfoImpl userId name phoneNumber pool =
    runSqlPoolOneConnection (patchUserInfo' userId name phoneNumber) pool

patchUserInfo'Impl :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => UserId -> Maybe String -> Maybe String -> PaperAuthConn -> PaperMonad p m NoContent
patchUserInfo'Impl userId name phoneNumber conn =
    undefined

enrollImpl :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => Config -> EncodeSigner -> String -> String -> String -> String -> PaperAuthPool -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
enrollImpl config encodeSigner paperId password phoneNumber phoneNumberSecret pool =
    runSqlPoolOneConnection (enroll' config encodeSigner paperId password phoneNumber phoneNumberSecret pool) pool

enroll'Impl :: forall p m. (HasCallStack, UserServiceI p, MonadUnliftIO m) => Config -> EncodeSigner -> String -> String -> String -> String -> PaperAuthPool -> PaperAuthConn -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
enroll'Impl config encodeSigner paperId password phoneNumber' phoneNumberSecret pool conn = do
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
    sameUserIdEntityList <- User.Repository.findByPaperId paperId conn
    case sameUserIdEntityList of
        [] -> return ()
        _ ->
            toPaperMonad $ PaperError "paperId duplicate" (err400 { errBody = "paperId duplicate" }) (callStack' profile)
    samePhoneNumberList <- User.Repository.findByPhoneNumber phoneNumber conn
    case samePhoneNumberList of
        [] -> return ()
        _ ->
            toPaperMonad $ PaperError "phoneNumber duplicate" (err400 { errBody = "phoneNumber duplicate" }) (callStack' profile)
    hashedPassword <- maybeTToPaperMonadUnliftIO
        (MaybeT $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (Data.ByteString.Char8.pack password))
        $ PaperError "hashing string error" (err500 { errBody = "internal server error" }) (callStack' profile)
    userId <- User.Repository.newUser Paper (Just paperId) (Just hashedPassword) (Just phoneNumber) Nothing currentUTC conn
    let roleSet = Data.Set.empty
        preAuthenticatedUser = PreAuthenticatedUser { userId, roleSet }
    JWTDTO { accessToken, refreshToken } <- JWT.ExService.issueJWT config encodeSigner preAuthenticatedUser currentUTC conn
    let cookie = generateRefreshTokenCookie (Proxy :: Proxy p) refreshToken
    return $ addHeader cookie $ EnrollResDTO { accessToken }
    where
        inner :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => VerificationId -> Int -> PaperAuthConn -> PaperMonad p m ()
        inner verificationId failCount innerConn =
                if failCount == 4 then
                    Verification.Repository.deleteById innerConn verificationId
                else
                    Verification.Repository.increaseFailCount innerConn verificationId