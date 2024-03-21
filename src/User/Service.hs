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

import qualified JWT.ExService
import JWT.ExService(JWTExServiceI)
import qualified User.ExService
import User.ExService(UserExServiceI)
import qualified User.Repository
import User.Repository(UserRepositoryI)
import qualified Verification.ExService
import Verification.ExService(VerificationExServiceI)

import JWT.Util
import JWT.Model
import User.DTO
import User.Entity
import Verification.Util
import DB
import Enum
import PaperMonad
import CallStack

import Servant
import Web.JWT
import Web.Cookie
import Data.Configurator.Types

import Control.Monad.Reader
import Control.Monad.IO.Unlift
import Data.Set hiding (deleteAt)
import Data.Time
import GHC.Stack

class (DBI p, JWTUtilI p, UserExServiceI p, UserRepositoryI p, VerificationExServiceI p, JWTExServiceI p, VerificationUtilI p) => UserServiceI p where
    getUserInfo :: (HasCallStack, MonadUnliftIO m) => UserId -> PaperAuthPool -> PaperMonad p m GetUserInfoResDTO
    getUserInfo = getUserInfoImpl
    getUserInfo' :: (HasCallStack, MonadUnliftIO m) => UserId -> PaperAuthConn -> PaperMonad p m GetUserInfoResDTO
    getUserInfo' = getUserInfo'Impl
    patchUserInfo :: (HasCallStack, MonadUnliftIO m) => UserId -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> PaperAuthPool -> PaperMonad p m NoContent
    patchUserInfo = patchUserInfoImpl
    patchUserInfo' :: (HasCallStack, MonadUnliftIO m) => UserId -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> PaperAuthPool -> PaperAuthConn -> PaperMonad p m NoContent
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

patchUserInfoImpl :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => UserId -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> PaperAuthPool -> PaperMonad p m NoContent
patchUserInfoImpl userId paperId password name phoneNumber phoneNumberSecret pool =
    runSqlPoolOneConnection (patchUserInfo' userId paperId password name phoneNumber phoneNumberSecret pool) pool

patchUserInfo'Impl :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => UserId -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> PaperAuthPool -> PaperAuthConn -> PaperMonad p m NoContent
patchUserInfo'Impl userId paperId' password' name' phoneNumber'' phoneNumberSecret' pool conn = do
    profile <- ask
    case paperId' of
        Just paperId -> Verification.ExService.verifyPaperId paperId conn
        Nothing -> return ()
    phoneNumberMaybe <- case (phoneNumber'', phoneNumberSecret') of
        (Just phoneNumber', Just phoneNumberSecret) -> do
            phoneNumber <- stringToPhoneNumber phoneNumber'
            Verification.ExService.verifyVerification phoneNumber phoneNumberSecret pool conn
            Verification.ExService.verifyPhoneNumber phoneNumber conn
            return $ Just phoneNumber
        (Just _, Nothing) -> toPaperMonad $ PaperError "try to patch phoneNumber without phoneNumberSecret" (err400 { errBody = "missing phoneNumberSecret" }) $ callStack' profile
        _ -> return Nothing
    hashedPassword' <- case password' of
        Just password -> Just <$> User.ExService.hashPassword password
        Nothing -> return Nothing
    User.Repository.patchUserInfo userId paperId' hashedPassword' name' phoneNumberMaybe conn
    return NoContent

enrollImpl :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => Config -> EncodeSigner -> String -> String -> String -> String -> PaperAuthPool -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
enrollImpl config encodeSigner paperId password phoneNumber phoneNumberSecret pool =
    runSqlPoolOneConnection (enroll' config encodeSigner paperId password phoneNumber phoneNumberSecret pool) pool

enroll'Impl :: (HasCallStack, UserServiceI p, MonadUnliftIO m) => Config -> EncodeSigner -> String -> String -> String -> String -> PaperAuthPool -> PaperAuthConn -> PaperMonad p m (Headers '[Header "Set-Cookie" SetCookie] EnrollResDTO)
enroll'Impl cfg encodeSigner paperId password phoneNumber' phoneNumberSecret pool conn = do
    profile <- ask
    currentUTC <- paperLiftIOUnliftIO getCurrentTime
    phoneNumber <- stringToPhoneNumber phoneNumber'
    Verification.ExService.verifyVerification phoneNumber phoneNumberSecret pool conn
    Verification.ExService.verifyPaperId paperId conn
    Verification.ExService.verifyPhoneNumber phoneNumber conn
    hashedPassword <- User.ExService.hashPassword password
    userId <- User.Repository.newUser Paper (Just paperId) (Just hashedPassword) (Just phoneNumber) Nothing currentUTC conn
    let roleSet = Data.Set.empty
        preAuthenticatedUser = PreAuthenticatedUser { userId, roleSet }
    JWTDTO { accessToken, refreshToken } <- JWT.ExService.issueJWT cfg encodeSigner preAuthenticatedUser currentUTC conn
    let cookie = generateRefreshTokenCookie profile refreshToken
    return $ addHeader cookie $ EnrollResDTO { accessToken }