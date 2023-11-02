{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text as TS
import qualified Html as HTML
import Html.Types (Node)
import Views.Layout
import Web.Twain

data LayoutType = AnonymousLayout | AuthenticatedLayout

getLayout :: LayoutType -> Layout
getLayout AnonymousLayout = anonymousLayout
getLayout AuthenticatedLayout = anonymousLayout

respondWithHtmlNode :: LayoutType -> TL.Text -> Node -> ResponderM a
respondWithHtmlNode layoutType title node = do
    currentPath <- getCurrentPath
    isHtmx <- checkHtmxRequest

    let response =
            if isHtmx then
                HTML.renderNode node
            else
                HTML.renderDocument $ getLayout layoutType currentPath title [node]

    send $ html response

getCurrentPath :: ResponderM TL.Text
getCurrentPath = do TLE.decodeUtf8 . BL.fromStrict . rawPathInfo <$> request

respondWithHtmlNode' :: Bool -> Node -> ResponderM a
respondWithHtmlNode' isHtmx node = do
    let render = if isHtmx then HTML.renderNode else HTML.renderDocument
    send $ html $ render node


checkHtmxRequest :: ResponderM Bool
checkHtmxRequest =
    header "Hx-Request" >>= parseHtmxHeader
        where 
            parseHtmxHeader :: Maybe TS.Text -> ResponderM Bool
            parseHtmxHeader maybeValue = do
                return $ case maybeValue of 
                    Just value -> value == "true"
                    Nothing -> False
