-- {-# LANGUAGE OverloadedStrings #-}

module Handlers.Authentication where

import qualified Html as HTML
import Pages.Authentication
import Web.Twain

handleGetSignIn :: ResponderM a
handleGetSignIn = send $ html $ HTML.renderDocument signInPage

