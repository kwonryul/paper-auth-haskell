{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Definition(
    defineDTO
  , defineEnum
) where

import Language.Haskell.TH

import Control.Exception
import System.Environment

defineDTO :: String -> Q[Dec]
defineDTO path = do
    contents <- runIO $ do
        homeDir <- getEnv "HOME"
        projectDir <- readFile $ homeDir ++ "/.paper-auth/project-directory"
        let filePath = projectDir ++ "definitions/" ++ path
        readFile filePath
    (dtoName, varBangTypes) <-
        case filter (not . null) $ lines contents of
            [] -> runIO $ throwIO $ userError "parse error" 
            (dtoName : vbts) -> do
                varBangTypes <- runIO $ toVarBangTypes vbts
                return (dtoName, varBangTypes)
    return $ [
        DataD [] (mkName dtoName) [] Nothing
            [ RecC (mkName dtoName) varBangTypes ] [
                DerivClause Nothing [ConT ''Show, ConT ''Read, ConT ''Eq]
            ]
        ]

toVarBangTypes :: [String] -> IO [VarBangType]
toVarBangTypes input = inner input False
    where
        inner :: [String] -> Bool -> IO [VarBangType]
        inner [] _ = return []
        inner (h : t) False =
            case words h of
                (name : bt : _) -> do
                    (b, typ) <-
                        case bt of
                            ('!' : typ') -> do
                                typ <- getType typ'
                                return (Bang NoSourceUnpackedness SourceStrict , typ)
                            ('~' : typ') -> do
                                typ <- getType typ'
                                return (Bang NoSourceUnpackedness SourceLazy, typ)
                            _ -> do
                                typ <- getType bt
                                return (Bang NoSourceUnpackedness NoSourceStrictness , typ)
                    ((mkName name, b, typ) :) <$> inner t True
                _ -> throwIO $ userError "parse error"
        inner (h : t) True =
            if h == "-" then
                inner t False
            else
                inner t True
        getType :: String -> IO Type
        getType "?" = throwIO $ userError "parse error"
        getType ('?' : t) = AppT (ConT ''Maybe) <$> getType t
        getType "[]" = throwIO $ userError "parse error"
        getType ('[' : t) =
            case reverse t of
                (']' : rev) -> AppT ListT <$> getType (reverse rev)
                _ -> throwIO $ userError "parse error"
        getType t = return $ ConT $ mkName t

defineEnum :: String -> Q[Dec]
defineEnum path = do
    contents <- runIO $ do
        homeDir <- getEnv "HOME"
        projectDir <- readFile $ homeDir ++ "/.paper-auth/project-directory"
        let filePath = projectDir ++ "definitions/" ++ path
        readFile filePath
    (enumName, cons) <-
        case filter (not . null) $ lines contents of
            [] -> runIO $ throwIO $ userError "parse error"
            (enumName : cs) -> do
                cons <- runIO $ toConstructors cs
                return (enumName, cons)
    return $ [
        DataD [] (mkName enumName) [] Nothing cons [
            DerivClause Nothing [ConT ''Show, ConT ''Read, ConT ''Eq]
            ]
        ]

toConstructors :: [String] -> IO [Con]
toConstructors input = inner input False
    where
        inner :: [String] -> Bool -> IO [Con]
        inner [] _ = return []
        inner (h : t) False =
            (NormalC (mkName h) [] :)  <$> inner t True
        inner (h : t) True =
            if h == "-" then
                inner t False
            else
                inner t True