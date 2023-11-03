{-# LANGUAGE OverloadedStrings #-}
module Controllers.Root where

import Utils
import Views.Root
import Web.Twain

handleGetGuestIndex :: ResponderM a
handleGetGuestIndex = do
    isAuth <- isAuthenticated
    let layout = if isAuth then AuthenticatedLayout else AnonymousLayout

    respondWithHtmlNode layout "Home" guestLandingPage
