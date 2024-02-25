{-# LANGUAGE DataKinds #-}

module User.Controller(

) where

import Servant

{-
type API = "user" :> (
    Enroll
    :<|> Login
    :<|> RefreshToken
    :<|> Withdrawal
    :<|> "info" :> (
            GetUserInfo
            :<|> PatchUserInfo
        )
    )
-}

type Enroll = "enroll" :> Post '[JSON] (Headers '[Header "Authorization" String] NoContent)