{-# LANGUAGE OverloadedStrings #-}

module Views.Authentication where

import Data.Text.Lazy (Text)
import Html
import Html.Attributes
import Html.Types
import Views.Layout
import Views.Components (formInput, formTextInput, formPasswordInput)

signInPage :: Bool -> Text -> Node
signInPage isHtmx path =
    let
        content = 
            div'
                [ id' "sign-in-page" ]
                [ form
                    []
                    [ formTextInput "email_address" "Email Address" ""
                    , formPasswordInput "password" "Password"
                    ]
                ]
    in
        if isHtmx then content else anonymousLayout path "Sign In" [content]
