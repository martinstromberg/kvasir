{-# LANGUAGE OverloadedStrings #-}

module Controllers.Authentication where

import Views.Authentication
import Web.Twain
import Utils

handleGetSignIn :: ResponderM a
handleGetSignIn = do
    respondWithHtmlNode AnonymousLayout "Sign In" signInPage

