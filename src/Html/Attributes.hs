{-# LANGUAGE OverloadedStrings #-}
module Html.Attributes where

import Data.Text.Lazy (Text, intercalate, unwords)
import Html.Types

keyOf :: Attribute -> AttributeKey
keyOf (key, _) = key

valueOf :: Attribute -> AttributeValue
valueOf (_, val) = val

attribute :: AttributeKey -> AttributeValue -> Attribute
attribute key value = (key, value)

toText :: Attribute -> Text
toText attr =
    let 
        k = keyOf attr
        v = valueOf attr
    in intercalate "" [k, "=\"", v, "\""]

listToText :: [Attribute] -> Text
listToText = Data.Text.Lazy.unwords . map toText

-- Shorthands
class' :: Text -> Attribute
class' = attribute "class"

crossOrigin :: Text -> Attribute
crossOrigin = attribute "crossorigin"

href :: Text -> Attribute
href = attribute "href"

hxBoost :: Bool -> Attribute
hxBoost True = attribute "hx-boost" "true"
hxBoost False = attribute "hx-boost" "false"

hxGet :: Text -> Attribute
hxGet = attribute "hx-get"

hxSwap :: Text -> Attribute
hxSwap = attribute "hx-swap"

hxTarget :: Text -> Attribute
hxTarget = attribute "hx-target"

id' :: Text -> Attribute
id' = attribute "id"

integrity :: Text -> Attribute
integrity = attribute "integrity"

lang :: Text -> Attribute
lang = attribute "lang"

media :: Text -> Attribute
media = attribute "media"

name :: Text -> Attribute
name = attribute "name"

rel :: Text -> Attribute
rel = attribute "rel"

role' :: Text -> Attribute
role' = attribute "role"

src :: Text -> Attribute
src = attribute "src"

type' :: Text -> Attribute
type' = attribute "type"

value :: Text -> Attribute
value = attribute "value"
