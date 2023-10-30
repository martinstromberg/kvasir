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

id' :: Text -> Attribute
id' = attribute "id"

integrity :: Text -> Attribute
integrity = attribute "integrity"

lang :: Text -> Attribute
lang = attribute "lang"

src :: Text -> Attribute
src = attribute "src"
