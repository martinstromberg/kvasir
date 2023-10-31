{-# LANGUAGE OverloadedStrings #-}

module Controllers.Root where

import qualified Data.Text as TS
import Data.Text.Lazy.Encoding as TLE (decodeUtf8)
import Data.ByteString.Lazy as BL (fromStrict) 
import qualified Html as HTML
import Views.Root
import Web.Twain


parseHtmxHeader :: Maybe TS.Text -> ResponderM Bool
parseHtmxHeader maybeValue = do
    return $ case maybeValue of 
        Just value -> value == "true"
        Nothing -> False

handleGetGuestIndex :: ResponderM a
handleGetGuestIndex = do
    req <- request
    let currentPath = decodeUtf8 $ fromStrict $ rawPathInfo req

    isHtmx <- header "Hx-Request" >>= parseHtmxHeader

    let render = if isHtmx then HTML.renderNode else HTML.renderDocument
    send $ html $ render $ guestLandingPage isHtmx currentPath
