{-# LANGUAGE OverloadedStrings #-}
module Views.Root where

import Data.Text.Lazy (Text)
import Html
import Html.Attributes
import Html.Types
import Views.Layout

guestLandingPage :: Node
guestLandingPage =
    text "This is the landing page"
