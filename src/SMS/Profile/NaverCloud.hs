{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SMS.Profile.NaverCloud(
    SMSNaverCloud
) where

import ThirdParties.NaverCloud.ExService
import SMS.ExService
import SMS.Profile
import Verification.Util
import CallStack
import Configurator
import PaperMonad

import Servant
import Servant.Client
import Network.HTTP.Client.TLS
import Data.Aeson.TH
import Data.Configurator.Types

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Text
import Data.Time
import GHC.Stack

data SMSNaverCloud
instance SMSProfile SMSNaverCloud

data ToDTO = ToDTO {
    to :: String
  } deriving Show
$(deriveJSON defaultOptions ''ToDTO)

data SMSMessageReqDTO = SMSMessageReqDTO {
    type' :: String
  , from :: String
  , content :: String
  , messages :: [ToDTO]
  } deriving Show
$(deriveJSON defaultOptions { fieldLabelModifier = (\name -> if name == "type'" then "type" else name) } ''SMSMessageReqDTO)

instance (SMSProfileC p, SMSProfileF p ~ SMSNaverCloud, ConfiguratorI p, NaverCloudExServiceI p) => SMSExServiceI p where
    smsNotify = smsNotifyImpl

type SMSMessageC = "sms" :> "v2" :> "services" :> Capture "serviceId" String :> "messages" :>
    Servant.Header "X-NCP-APIGW-TIMESTAMP" String :> Servant.Header "X-NCP-IAM-ACCESS-KEY" String :> Servant.Header "X-NCP-APIGW-SIGNATURE-V2" String :>
    ReqBody '[JSON] SMSMessageReqDTO :> (Verb 'POST 202) '[JSON] NoContent

smsMessageC :: Client ClientM SMSMessageC
smsMessageC = client (Servant.Proxy :: Servant.Proxy SMSMessageC)

smsNotifyImpl :: (HasCallStack, SMSExServiceI p, SMSProfileF p ~ SMSNaverCloud, ConfiguratorI p, NaverCloudExServiceI p, MonadUnliftIO m) => Config -> PhoneNumber -> String -> PaperMonad p m ()
smsNotifyImpl cfg phoneNumber msg = do
    profile <- ask
    currentUTC <- paperLiftIOUnliftIO getCurrentTime
    accessKey <- lookupRequired cfg "third-parties.naver-cloud.access-key"
    secretKey <- lookupRequired cfg "third-parties.naver-cloud.secret-key"
    serviceId <- lookupRequired cfg "third-parties.naver-cloud.sms.service-id"
    from <- lookupRequired cfg "third-parties.naver-cloud.sms.from"
    baseUrl' <- paperLiftIOUnliftIO $ parseBaseUrl "https://sens.apigw.ntruss.com"
    let urlPath = "/sms/v2/services/" ++ serviceId ++ "/messages"
        smsMessageCWithHeaders = applyNaverCloudHeaders profile accessKey secretKey POST urlPath currentUTC (smsMessageC serviceId)
        body = SMSMessageReqDTO {
            type' = "LMS"
          , from = from
          , content = toContent profile msg
          , messages = [ToDTO { to = toTo profile phoneNumber }]
        }
    manager <- paperLiftIOUnliftIO $ newTlsManager
    let clientEnv = mkClientEnv manager baseUrl'
    result <- paperLiftIOUnliftIO $ runClientM (smsMessageCWithHeaders body) clientEnv
    case result of
        Left err -> toPaperMonad $ PaperCatchError err (err500 { errBody = "naver cloud connection error" }) (callStack' profile)
        Right NoContent -> return ()

toContent :: (SMSExServiceI p, SMSProfileF p ~ SMSNaverCloud) => Servant.Proxy p -> String -> String
toContent _ msg = "[Paper]\n" ++ (Data.Text.unpack $ Data.Text.pack msg)

toTo :: (SMSExServiceI p, SMSProfileF p ~ SMSNaverCloud) => Servant.Proxy p -> PhoneNumber -> String
toTo _ (PhoneNumber phoneNumber) = Prelude.filter (\x -> x /= '-') phoneNumber