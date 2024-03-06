{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Profile.Test() where

import JWT.Controller
import JWT.Repository
import JWT.Service
import JWT.Util
import Role.Repository
import User.Controller
import User.Repository
import User.Service
import Verification.DTO
import Verification.Repository
import Verification.Service
import Verification.Util
import Authentication
import Context
import DB
import GlobalMonad
import PaperApp
import Lib

import Paths_paper_auth

import Profile.Test.Import
import Profile.Test.Snippet

import Data.Configurator

instance JWTControllerI Test
instance JWTRepositoryI Test
instance JWTServiceI Test
instance JWTUtilI Test
instance RoleRepositoryI Test
instance UserControllerI Test
instance UserRepositoryI Test
instance UserServiceI Test
instance VerificationDTOI Test
instance VerificationRepositoryI Test
instance VerificationServiceI Test
instance VerificationUtilI Test
instance AuthenticationI Test
--instance ContextI Test
instance DBI Test
instance LibI Test

instance ContextI Test where
    getConfig' = do
        filePath <- globalLiftIOUnliftIO $ getDataFileName "resources/application-test.cfg"
        globalLiftIOUnliftIO $ autoReload autoConfig [Required filePath]

instance PaperAppI Test where
    app p context docsFilePath staticFilePath = generateSnippetM context $ appImpl p context docsFilePath staticFilePath