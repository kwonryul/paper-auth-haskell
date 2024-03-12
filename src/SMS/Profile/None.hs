{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module SMS.Profile.None(
    SMSNone
) where

import PaperMonad
import SMS.ExService
import SMS.Profile

data SMSNone
instance SMSProfile SMSNone

instance (SMSProfileC p, SMSProfileF p ~ SMSNone, PaperMonadI p) => SMSExServiceI p where
    smsNotify _ _ _ = return ()