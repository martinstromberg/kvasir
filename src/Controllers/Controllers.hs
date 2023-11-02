{-# LANGUAGE OverloadedStrings #-}

module Controllers where

import Controllers.Authentication
import Controllers.Root
import Database.SQLite.Simple (Connection)
import Web.Twain
import Types (OAuth2Info)

routes :: Connection -> OAuth2Info -> [Middleware]
routes conn oauth2Info =
    [ get "/" handleGetGuestIndex
    , get "/sign-in" $ handleGetSignIn oauth2Info
    , get "/oauth2/callback" $ handleGetOAuth2Callback oauth2Info
    ]

