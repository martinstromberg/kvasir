{-# LANGUAGE OverloadedStrings #-}

module Views.Layout where

import Data.Text.Lazy as TL (Text, isPrefixOf, unwords)
import Html
import Html.Attributes
import Html.Types

type Layout = Text -> Text -> [Node] -> Node

htmxNode :: Node
htmxNode = 
    script
        [ crossOrigin "anonymous"
        , integrity "sha384-FhXw7b6AlE/jyjlZH5iHa/tTe9EpJ1Y55RjcgPbjeWMskSxZt1v9qkxLJWNJaGni"
        , src "https://unpkg.com/htmx.org@1.9.6"
        ] ""

commonLayout :: Text -> [Node] -> Node
commonLayout pageTitle bodyContent = 
    html
        [ lang "en" ]
        [ head'
            [ meta [attribute "charset" "utf-8"]
            , title $ (<>) pageTitle " - Kvasir"
            , link
                [ href "/kvasir.css"
                , media "all"
                , rel "stylesheet"
                , type' "text/css"
                ]
            ]
        , body [] bodyContent
        ]

activeClass :: Text -> Text -> Text -> Text
activeClass href path classes
    | href `isPrefixOf` path = TL.unwords [classes, "active"]
    | otherwise = classes
    

anonymousHeader :: Text -> Node
anonymousHeader activePath = 
    header [ id' "guest-header" , hxBoost True ]
    [ div' []
        [ a' 
            [ href "/"
            , hxSwap "innerHTML"
            , hxTarget "#app-root"
            ]
            [ text "Kvasir" ]
        ]
    , nav [ id' "guest-menu"]
        [ ul []
            [ li
                [ class' $ activeClass "/sign-in" activePath "menu-item"
                ]
                [ a'
                    [ href "/sign-in"
                    , hxBoost False
                    ]
                    [ text "Sign In" ]
                ]
            ]
        ]
    ]

anonymousLayout :: Layout
anonymousLayout path pageTitle elems =
    commonLayout pageTitle
    [ anonymousHeader path
    , main' [ id' "app-root" ] elems
    , footer [] [text "this is my footer"]
    , htmxNode
    ]

authenticatedLayout :: Layout
authenticatedLayout path pageTitle elems =
    commonLayout pageTitle
    [ header [] [text "this is the authenticated header"]
    , main' [ id' "app-root" ] elems
    , footer [] [text "this is my footer"]
    , htmxNode
    ]

