{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module SMS.Profile.None(
    SMSNone
) where

import PaperMonad
import SMS.Profile
import SMS.Service

data SMSNone
instance SMSProfile SMSNone

instance (SMSProfileC p, SMSProfileF p ~ SMSNone, PaperMonadI p) => SMSServiceI p where
    smsNotify _ _ _ = return ()