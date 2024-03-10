module Util(
    UtilI(
        toCurrentTimeMillis
      )
) where

import Monad.ProfileT

import Data.Time
import Data.Proxy

class Profile p => UtilI p where
    toCurrentTimeMillis :: Proxy p -> UTCTime -> Int
    toCurrentTimeMillis = toCurrentTimeMillisImpl

toCurrentTimeMillisImpl :: (Profile p) => Proxy p -> UTCTime -> Int
toCurrentTimeMillisImpl _ utcTime = floor $ (diffUTCTime utcTime $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)) * 1000