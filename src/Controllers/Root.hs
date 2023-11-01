module Controllers.Root where

import Utils
import Views.Root
import Web.Twain

handleGetGuestIndex :: ResponderM a
handleGetGuestIndex = do
    currentPath <- getCurrentPath
    isHtmx <- checkHtmxRequest

    respondWithHtmlNode isHtmx $ guestLandingPage isHtmx currentPath
