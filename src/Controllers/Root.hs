{-# LANGUAGE OverloadedStrings #-}
module Controllers.Root where

import Utils
import Views.Root
import Web.Twain

handleGetGuestIndex :: ResponderM a
handleGetGuestIndex = do
    respondWithHtmlNode AnonymousLayout "Home" guestLandingPage
