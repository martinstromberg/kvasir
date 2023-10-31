{-# LANGUAGE OverloadedStrings #-}
module Views.Root where

import Data.Text.Lazy (Text)
import Html
import Html.Attributes
import Html.Types
import Views.Layout

guestLandingPage :: Text -> Node
guestLandingPage path =
    anonymousLayout path ""
    [ text "This is the landing page"
    ]
