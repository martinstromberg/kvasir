module Controllers.Authentication where

import Views.Authentication
import Web.Twain
import Utils

handleGetSignIn :: ResponderM a
handleGetSignIn = do
    currentPath <- getCurrentPath
    isHtmx <- checkHtmxRequest

    respondWithHtmlNode isHtmx $ signInPage isHtmx currentPath

