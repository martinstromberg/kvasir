{-# LANGUAGE OverloadedStrings #-}

module Pages.Layout where

import Data.Text.Lazy (Text)
import Html
import Html.Attributes
import Html.Types

anonymousLayout :: Text -> [Node] -> Node
anonymousLayout pageTitle elems =
    html
        [ lang "en" ]
        [ head'
            [ meta [attribute "charset" "utf-8"]
            , title pageTitle
            ]
        , body []
            [ header [] [text "this is my header"]
            , main' [] elems
            , footer [] [text "this is my footer"]
            , script
                [ crossOrigin "anonymous"
                , integrity "sha384-FhXw7b6AlE/jyjlZH5iHa/tTe9EpJ1Y55RjcgPbjeWMskSxZt1v9qkxLJWNJaGni"
                , src "https://unpkg.com/htmx.org@1.9.6"
                ] ""
            ]
        ]
