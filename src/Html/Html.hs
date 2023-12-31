{-# LANGUAGE OverloadedStrings #-}
module Html where

import Data.Either
import Data.Text.Lazy (Text, intercalate)
import qualified Data.Text.Lazy.Encoding as TLE
import Data.ByteString.Lazy as BSL (ByteString)
import qualified Html.Attributes as Attrs (listToText)
import Html.Types

tagNameOf :: Element -> TagName
tagNameOf (Element { tagName = tn }) = tn

attributesOf :: Element -> [Attribute]
attributesOf (Element { attributes = attrs }) = attrs

childrenOf :: Element -> [Node]
childrenOf (Element { children = n }) = n

element :: TagName -> [Attribute] -> [Node] -> Node
element tn attrs nodes = 
    let el = Element
                { tagName = tn
                , attributes = attrs
                , children = nodes
                }
    in Right el

text :: Text -> Node
text = Left

-- Rendering
nodeToText :: Node -> Text
nodeToText (Left txt) = txt
nodeToText (Right el) = elementToText el

elementToText :: Element -> Text
elementToText n =
    let tn = tagNameOf n
        attrs = attributesOf n
        attrText = Attrs.listToText attrs
        attrSep = case attrs of
                    [] -> ""
                    _ -> " "
        nodes = childrenOf n
        txt = case nodes of
                [] -> ["<", tn, attrSep, attrText, " />"]
                _ -> ["<", tn, attrSep, attrText, ">", nodesToText nodes, "</", tn, ">"]
    in intercalate "" txt

nodesToText :: [Node] -> Text
nodesToText = intercalate "" . map nodeToText

renderNode :: Node -> ByteString
renderNode = TLE.encodeUtf8 . nodeToText

renderNodes :: [Node] -> ByteString
renderNodes = TLE.encodeUtf8 . nodesToText

renderDocument :: Node -> ByteString
renderDocument n =
    TLE.encodeUtf8 $ intercalate "" ["<!DOCTYPE html>", nodeToText n]

-- Shorthands
a' :: [Attribute] -> [Node] -> Node
a' = element "a"

body :: [Attribute] -> [Node] -> Node
body = element "body"

button :: [Attribute] -> [Node] -> Node
button = element "button"

div' :: [Attribute] -> [Node] -> Node
div' = element "div"

footer :: [Attribute] -> [Node] -> Node
footer = element "footer"

form :: [Attribute] -> [Node] -> Node
form = element "form"

h1 :: [Attribute] -> [Node] -> Node
h1 = element "h1"

head' :: [Node] -> Node
head' = element "head" []

header :: [Attribute] -> [Node] -> Node
header = element "header"

html :: [Attribute] -> [Node] -> Node
html = element "html"

input :: [Attribute] -> Node
input attrs = element "input" attrs []

label :: [Attribute] -> [Node] -> Node
label = element "label"

li :: [Attribute] -> [Node] -> Node
li = element "li"

link :: [Attribute] -> Node
link attrs = element "link" attrs []

main' :: [Attribute] -> [Node] -> Node
main' = element "main"

meta :: [Attribute] -> Node
meta attrs = element "meta" attrs []

nav :: [Attribute] -> [Node] -> Node
nav = element "nav"

section :: [Attribute] -> [Node] -> Node
section = element "section"

script :: [Attribute] -> Text -> Node
script attrs txt = element "script" attrs [text txt]

title :: Text -> Node
title t = element "title" [] [text t]

ul :: [Attribute] -> [Node] -> Node
ul = element "ul"

