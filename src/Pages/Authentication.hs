{-# LANGUAGE OverloadedStrings #-}

module Pages.Authentication where

import Data.Text.Lazy (Text)
import Html
import Html.Attributes
import Html.Types
import Pages.Layout

signInPage :: Node
signInPage =
    anonymousLayout "Sign In" 
    [ div' [] [text "this is the signin page"]
    ]
