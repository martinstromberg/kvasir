{-# LANGUAGE OverloadedStrings #-}
module Views.Root where

import Data.Text.Lazy (Text)
import Html
import Html.Attributes
import Html.Types
import Views.Layout

guestLandingPage :: Bool -> Text -> Node
guestLandingPage htmx path =
    let content = text "This is the landing page"
    in if htmx then content else anonymousLayout path "Home" [content]
