module Controllers.Root where

import Data.Text.Lazy.Encoding as TLE (decodeUtf8)
import Data.ByteString.Lazy as BL (fromStrict) 
import qualified Html as HTML
import Views.Root
import Web.Twain

handleGetGuestIndex :: ResponderM a
handleGetGuestIndex = do
    req <- request
    let currentPath = decodeUtf8 $ fromStrict $ rawPathInfo req
    send $ html $ HTML.renderDocument $ guestLandingPage currentPath
