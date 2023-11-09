{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust, isJust)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text as TS
import Database.Beam (Identity)
import Database.SQLite.Simple (Connection)
import Database.Types
import qualified Html as HTML
import Html.Types (Node)
import Views.Layout
import Web.JWT as JWT
import Web.Twain as Twain
import Database.Page (getTopPagesByCreator)

data LayoutType = AnonymousLayout | AuthenticatedLayout

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

getAuthenticatedLayout :: Connection -> AccountT Identity -> TL.Text -> TL.Text -> [Node] -> IO Node
getAuthenticatedLayout conn acc path title els = do
    let accountId = _accountId acc

    pages <- getTopPagesByCreator accountId conn
    
    return $ authenticatedLayout acc pages path title els

respondWithHtmlNode :: Connection -> Maybe (AccountT Identity) -> TL.Text -> Node -> ResponderM a
respondWithHtmlNode conn mAcc title node = do
    currentPath <- getCurrentPath
    isHtmx <- checkHtmxRequest

    response' <- liftIO $ response isHtmx mAcc title currentPath node

    send $ html response'
    where
        response :: Bool -> Maybe (AccountT Identity) -> TL.Text -> TL.Text -> Node -> IO BL.ByteString
        response True _ _ _ node' = return $ HTML.renderNode node'
        response False mAcc' title' path' node' = do
            HTML.renderDocument <$> withLayout mAcc' title' path' node'

        withLayout :: Maybe (AccountT Identity) -> TL.Text -> TL.Text -> Node -> IO Node
        withLayout (Just account) title' path' node = do
            getAuthenticatedLayout conn account path' title' [node]
        withLayout _ title' path' node =
            return $ anonymousLayout path' title' [node]


getCurrentPath :: ResponderM TL.Text
getCurrentPath = do TLE.decodeUtf8 . BL.fromStrict . rawPathInfo <$> request

checkHtmxRequest :: ResponderM Bool
checkHtmxRequest =
    Twain.header "Hx-Request" >>= parseHtmxHeader
        where 
            parseHtmxHeader :: Maybe TS.Text -> ResponderM Bool
            parseHtmxHeader maybeValue = do
                return $ case maybeValue of 
                    Just value -> value == "true"
                    Nothing -> False
