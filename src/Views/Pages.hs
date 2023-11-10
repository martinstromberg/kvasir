{-# LANGUAGE OverloadedStrings #-}

module Views.Pages where

import qualified Data.Text.Lazy as TL
import Html
import Html.Attributes
import Html.Types
import Database.Types (PageT(..))
import Database.Beam (Identity)

singlePageView :: PageT Identity -> Node
singlePageView page =
    section [ class' "kvasir-page" ]
        [ div' [ class' "page-layout" ]
            [ h1 [ class' "page-title" ] 
                [ text $ TL.fromStrict $ _pageTitle page ]
            , div' [ class' "page-layout" ]
                [ text $ TL.fromStrict $ _pageBody page ]
            ]
        ]
