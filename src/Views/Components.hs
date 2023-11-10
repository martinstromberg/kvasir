{-# LANGUAGE OverloadedStrings #-}

module Views.Components where

import Data.Text.Lazy (Text, fromStrict)
import Database.Types
import Html
import Html.Attributes
import Html.Types

formInput :: Text -> Text -> Text -> Text -> Node
formInput type_ name_ placeholder val =
    div' [ class' "form-input" ]
    [ label []
        [ text placeholder
        , input
            [ name name_
            , type' type_
            , value val
            ]
        ]
    ]

formTextInput :: Text -> Text -> Text -> Node
formTextInput = formInput "text"

formPasswordInput :: Text -> Text -> Node
formPasswordInput name_ placeholder = formInput "password" name_ placeholder ""

sidebarPageListItem :: Page -> Node
sidebarPageListItem page = 
    let 
        pageHref = fromStrict $ "/pages/" <> _pageId page
    in
        li [ class' "sidebar-page-list-item" ]
        [ a' [ href pageHref , hxTarget "#main-content", hxSwap "innerHTML" ]
            [ text (fromStrict $ _pageTitle page) ]
        ]
