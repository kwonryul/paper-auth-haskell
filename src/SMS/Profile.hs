{-# LANGUAGE TypeFamilies #-}
module SMS.Profile(
    SMSProfile
  , SMSProfileC(
        SMSProfileF
      )
) where

import Data.Kind

class SMSProfile p

class SMSProfile (SMSProfileF p) => SMSProfileC p where
    type SMSProfileF p:: Type