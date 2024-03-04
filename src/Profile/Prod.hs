module Profile.Prod(
    Prod
) where

import Monad.ProfileT

import JWT.Controller
import JWT.Repository
import JWT.Service
import JWT.Util
import Role.Repository
import User.Controller
import User.Repository
import User.Service
import Verification.Repository
import Verification.Service
import Verification.Util
import Authentication
import Configurator
import Context
import DB
import Lib
import PaperApp

data Prod
instance Profile Prod

instance JWTControllerI Prod
instance JWTRepositoryI Prod
instance JWTServiceI Prod
instance JWTUtilI Prod
instance RoleRepositoryI Prod
instance UserControllerI Prod
instance UserRepositoryI Prod
instance UserServiceI Prod
instance VerificationRepositoryI Prod
instance VerificationServiceI Prod
instance VerificationUtilI Prod
instance AuthenticationI Prod
instance ConfiguratorI Prod
instance ContextI Prod
instance DBI Prod
instance LibI Prod
instance PaperAppI Prod