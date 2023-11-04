{-# LANGUAGE OverloadedStrings #-}
module Views.Root where

import Data.Text as TS
import Data.Text.Lazy as TL
import Html
import Html.Attributes
import Html.Types
import Views.Layout

guestLandingPage :: Node
guestLandingPage =
    text "This is the landing page"

authenticatedLandingPage :: TS.Text -> Node
authenticatedLandingPage name =
    text $ (<>) "Hello " $ TL.fromStrict name
    
