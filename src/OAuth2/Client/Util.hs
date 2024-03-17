module OAuth2.Client.Util(
    OAuth2ClientUtilI(
        generateState
      , getSocketIdFromState
      )
) where

import Monad.ProfileT
import Import

import Data.Text
import Data.Char
import Data.Proxy
import System.Random

class Profile p => OAuth2ClientUtilI p where
    generateState :: Proxy p -> OAuth2ClientSocketId' -> IO Text
    generateState = generateStateImpl
    getSocketIdFromState :: Proxy p -> String -> Maybe OAuth2ClientSocketId'
    getSocketIdFromState = getSocketIdFromStateImpl


generateStateImpl :: OAuth2ClientUtilI p => Proxy p -> OAuth2ClientSocketId' -> IO Text
generateStateImpl _ socketId = do
    gen <- newStdGen
    return $ pack $ show socketId ++ (Prelude.take 15 $ randomRs ('a', 'z') gen)

getSocketIdFromStateImpl :: OAuth2ClientUtilI p => Proxy p -> String -> Maybe OAuth2ClientSocketId'
getSocketIdFromStateImpl _ state =
    case Prelude.takeWhile isDigit state of
        [] -> Nothing
        str -> Just $ read str