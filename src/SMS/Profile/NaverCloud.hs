{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SMS.Profile.NaverCloud(
    SMSNaverCloud
) where

import Verification.Util
import PaperMonad
import Configurator
import CallStack
import SMS.Profile
import SMS.Service
import ThirdParties.NaverCloud.Service

import Servant
import Servant.Client
import Network.HTTP.Client.TLS
import Data.Aeson.TH
import Data.Configurator.Types

import Control.Monad.IO.Unlift
import Data.Text
import Data.Time
import GHC.Stack

data SMSNaverCloud
instance SMSProfile SMSNaverCloud

data ToDTO = ToDTO {
    to :: String
  } deriving Show
$(deriveJSON defaultOptions ''ToDTO)

data SMSMessageRequestDTO = SMSMessageRequestDTO {
    type' :: String
  , from :: String
  , content :: String
  , messages :: [ToDTO]
  } deriving Show
$(deriveJSON defaultOptions { fieldLabelModifier = (\name -> if name == "type'" then "type" else name) } ''SMSMessageRequestDTO)

instance (SMSProfileC p, SMSProfileF p ~ SMSNaverCloud, ConfiguratorI p, NaverCloudServiceI p) => SMSServiceI p where
    smsNotify = smsNotifyImpl

type SMSMessageC = "sms" :> "v2" :> "services" :> Capture "serviceId" String :> "messages" :>
    Servant.Header "X-NCP-APIGW-TIMESTAMP" String :> Servant.Header "X-NCP-IAM-ACCESS-KEY" String :> Servant.Header "X-NCP-APIGW-SIGNATURE-V2" String :>
    ReqBody '[JSON] SMSMessageRequestDTO :> (Verb 'POST 202) '[JSON] NoContent

smsMessageC :: Client ClientM SMSMessageC
smsMessageC = client (Servant.Proxy :: Servant.Proxy SMSMessageC)

smsNotifyImpl :: forall p m. (HasCallStack, SMSServiceI p, SMSProfileF p ~ SMSNaverCloud, ConfiguratorI p, NaverCloudServiceI p, MonadUnliftIO m) => Config -> PhoneNumber -> String -> PaperMonad p m ()
smsNotifyImpl cfg phoneNumber msg = do
    currentUTC <- paperLiftIOUnliftIO getCurrentTime
    accessKey <- lookupRequired cfg "third-parties.naver-cloud.access-key"
    secretKey <- lookupRequired cfg "third-parties.naver-cloud.secret-key"
    serviceId <- lookupRequired cfg "third-parties.naver-cloud.sms.service-id"
    from <- lookupRequired cfg "third-parties.naver-cloud.sms.from"
    baseUrl' <- paperLiftIOUnliftIO $ parseBaseUrl "https://sens.apigw.ntruss.com"
    let urlPath = "/sms/v2/services/" ++ serviceId ++ "/messages"
        smsMessageCWithHeaders = applyNaverCloudHeaders profile accessKey secretKey POST urlPath currentUTC (smsMessageC serviceId)
        body = SMSMessageRequestDTO {
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
    where
        profile :: Servant.Proxy p
        profile = Servant.Proxy

toContent :: (SMSServiceI p, SMSProfileF p ~ SMSNaverCloud) => Servant.Proxy p -> String -> String
toContent _ msg = "[Paper]\n" ++ (Data.Text.unpack $ Data.Text.pack msg)

toTo :: (SMSServiceI p, SMSProfileF p ~ SMSNaverCloud) => Servant.Proxy p -> PhoneNumber -> String
toTo _ (PhoneNumber phoneNumber) = Prelude.filter (\x -> x /= '-') phoneNumber