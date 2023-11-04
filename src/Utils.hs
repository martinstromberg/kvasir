{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust, isJust)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text as TS
import qualified Html as HTML
import Html.Types (Node)
import Views.Layout
import Web.JWT as JWT
import Web.Twain as Twain

data LayoutType = AnonymousLayout | AuthenticatedLayout

getLayout :: LayoutType -> Layout
getLayout AnonymousLayout = anonymousLayout
getLayout AuthenticatedLayout = authenticatedLayout

getAuthCoookieJwt :: ResponderM (Maybe (JWT VerifiedJWT))
getAuthCoookieJwt = do
    mc <- cookieParamMaybe "KvasirAuth" >>= parseCookie
    liftIO $ verifyJwt mc
    where
        verifyJwt :: Maybe TS.Text -> IO (Maybe (JWT VerifiedJWT))
        verifyJwt (Just input) = do
            rsaKey <- liftIO $ fromJust . readRsaSecret <$> BS.readFile "jwt.pem"
            let verifier = toVerify $ EncodeRSAPrivateKey rsaKey
            return $ JWT.decodeAndVerifySignature verifier input
        verifyJwt _ = return Nothing

        parseCookie :: Maybe TS.Text -> ResponderM (Maybe TS.Text)
        parseCookie maybeCookie = do return maybeCookie


isAuthenticated :: ResponderM Bool
isAuthenticated = do isJust <$> getAuthCoookieJwt

authenticatedId :: ResponderM (Maybe TS.Text)
authenticatedId = do getAccountId <$> getAuthCoookieJwt
    where
        getSubject :: JWT VerifiedJWT -> Maybe TS.Text
        getSubject jwt =
            case sub $ claims jwt of
                Just a -> Just $ TS.pack $ show a
                Nothing -> Nothing

        getAccountId :: Maybe (JWT VerifiedJWT) -> Maybe TS.Text
        getAccountId (Just jwt) = getSubject jwt
        getAccountId _ = Nothing

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
    Twain.header "Hx-Request" >>= parseHtmxHeader
        where 
            parseHtmxHeader :: Maybe TS.Text -> ResponderM Bool
            parseHtmxHeader maybeValue = do
                return $ case maybeValue of 
                    Just value -> value == "true"
                    Nothing -> False
