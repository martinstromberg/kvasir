{-# LANGUAGE OverloadedStrings #-}

module Controllers where

import Database.SQLite.Simple (Connection)
import Web.Twain
import Controllers.Authentication (handleGetSignIn)
import Controllers.Root (handleGetGuestIndex)

routes :: Connection -> [Middleware]
routes conn =
    [ get "/" handleGetGuestIndex
    , get "/sign-in" handleGetSignIn
    ]

