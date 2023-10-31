-- {-# LANGUAGE OverloadedStrings #-}

module Controllers.Authentication where

import Data.Text.Lazy.Encoding as TLE (decodeUtf8)
import Data.ByteString.Lazy as BL (fromStrict) 
import qualified Html as HTML
import Views.Authentication
import Web.Twain

handleGetSignIn :: ResponderM a
handleGetSignIn = do
    req <- request
    let currentPath = decodeUtf8 $ fromStrict $ rawPathInfo req
    send $ html $ HTML.renderDocument $ signInPage currentPath

