{-# LANGUAGE OverloadedStrings #-}

module OAuth2.Client.HTML(
    issueJWTHtml
) where

import Text.Blaze.Html5 as Blaze
import Text.Blaze.Html5.Attributes as Blaze'

issueJWTHtml :: String -> Html
issueJWTHtml state = docTypeHtml $ do
    Blaze.head $ do
        Blaze.title "Paper-Auth"
        script ! type_ "text/javascript" $ do
            toMarkup $ concat [
                "fetch(\"/oauth2/client/finalize?state=" ++ state ++"\")"
              , ".then(res => { "
              , "window.close(); "
              , "});"
              ]
    body $ do
        h1 "Paper-Auth"
        "refreshToken has sent by cookie"
        br
        "now finalizing auth request"
        br
        "this window will be closed soon"