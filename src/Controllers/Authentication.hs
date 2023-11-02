{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Authentication where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as TS
import GHC.Generics (Generic)
import Network.URI.Encode
import Network.Wreq as Wreq
import Views.Authentication
import Web.Twain
import Utils
import Types (OAuth2Info (..))

handleGetSignIn :: OAuth2Info -> ResponderM a
handleGetSignIn oauth2Info = do
    send $ redirect302 redirectUri
    where
        authorizeUri :: TS.Text
        authorizeUri =  _idpUri oauth2Info <> ("/oauth2/authorize?" :: TS.Text)

        queryParams :: TS.Text
        queryParams = TS.intercalate "&"
            [ "client_id=" <> _clientId oauth2Info
            , "state="
            , "response_type=code"
            , "redirect_uri=" <> encodeText (_redirectUri oauth2Info <> "/oauth2/callback")
            , "scope=" <> _scope oauth2Info
            ]

        redirectUri :: TS.Text
        redirectUri = authorizeUri <> queryParams

data GetTokenBody = GetTokenBody
    { access_token :: TS.Text
    , token_type :: TS.Text
    } deriving (Show, Generic)

instance FromJSON GetTokenBody

redeemOAuth2Code :: OAuth2Info ->TS.Text -> IO (Wreq.Response GetTokenBody)
redeemOAuth2Code oauth2Info code = do
    asJSON =<< Wreq.post tokenUri
        [ "code" := code
        , "client_id" := _clientId oauth2Info
        , "client_secret" := _clientSecret oauth2Info
        , "grant_type" := ("code" :: TS.Text)
        , "scope" := _scope oauth2Info
        , "redirect_uri" := _redirectUri oauth2Info
        ]
    where
        tokenUri :: String
        tokenUri = TS.unpack $ _idpUri oauth2Info <> "/oauth2/token"

handleGetOAuth2Callback :: OAuth2Info -> ResponderM a
handleGetOAuth2Callback oauth2Info = do
    code <- getCode
    response <- liftIO $ redeemOAuth2Code oauth2Info code

    let accessToken = access_token (response ^. responseBody)

    send $ status status200 $ text accessToken
    where
        getCode :: ResponderM TS.Text
        getCode = queryParam "code"
