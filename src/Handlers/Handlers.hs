{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Database.SQLite.Simple (Connection)
import Web.Twain
import Handlers.Authentication (handleGetSignIn)

routes :: Connection -> [Middleware]
routes conn =
    [ get "/" $ index conn
    , get "/sign-in" handleGetSignIn
    ]

index :: Connection -> ResponderM a
index _ = send $ html "Hello, world!"

missing :: ResponderM a
missing = send $ html "Not found..."

