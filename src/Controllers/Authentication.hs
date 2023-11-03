{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Authentication where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import qualified Data.ByteString as BS 
import Database.SQLite.Simple (Connection)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TSE
import GHC.Generics (Generic)
import Network.URI.Encode
import Network.Wreq as Wreq
import Views.Authentication
import Web.Twain
import Utils
import Types (OAuth2Info (..))
import Database.Beam (runSelectReturningOne, select)
import Database.Account (getOrCreateAccount)
import Web.JWT as JWT
import Data.Maybe (fromJust)
import Database.Types (AccountT (_accountId))
import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime)

handleGetSignIn :: OAuth2Info -> ResponderM a
handleGetSignIn oauth2Info = do
    send $ redirect302 redirectUri
    where
        authorizeUri :: TS.Text
        authorizeUri =  _idpUri oauth2Info <> ("/oauth2/authorize?" :: TS.Text)

        queryParams :: TS.Text
        queryParams = TS.intercalate "&"
            [ "client_id=" <> _clientId oauth2Info
            , "state=" -- TODO: Add return URL
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

redeemOAuth2Code :: OAuth2Info -> TS.Text -> IO (Wreq.Response GetTokenBody)
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

data GetIdpUser = GetIdpUser
    { email_address :: TS.Text
    , first_name    :: TS.Text
    , last_name     :: TS.Text
    } deriving (Show, Generic)

instance FromJSON GetIdpUser

fetchUserInfo :: TS.Text -> TS.Text -> IO (Wreq.Response GetIdpUser)
fetchUserInfo idpUri token =
    asJSON =<< Wreq.getWith opts apiUrl 
    where
        authVal :: BS.ByteString
        authVal = TSE.encodeUtf8 $ "Bearer " <> token

        opts :: Wreq.Options
        opts = Wreq.defaults & Wreq.header "Authorization" .~ [authVal]

        apiUrl :: String
        apiUrl = TS.unpack idpUri <> "/api/v1/me"

getJwtToken :: AccountT Identity -> IO TS.Text
getJwtToken acc = do
    timestamp <- getPOSIXTime

    let cs = mempty
            { iss = stringOrURI . TS.pack $ "kvasir"
            , sub = stringOrURI $ _accountId acc
            , JWT.exp = numericDate (timestamp + 86400)
            , JWT.iat = numericDate timestamp
            , JWT.nbf = numericDate timestamp
            }

    rsaKey <- liftIO $ fromJust . readRsaSecret <$> BS.readFile "jwt.pem"
    return $ encodeSigned (EncodeRSAPrivateKey rsaKey) mempty cs

handleGetOAuth2Callback :: Connection -> OAuth2Info -> ResponderM a
handleGetOAuth2Callback conn oauth2Info = do
    -- TODO: Clean up this mess
    code <- getCode
    tokenResponse <- liftIO $ redeemOAuth2Code oauth2Info code

    let accessToken = access_token (tokenResponse ^. responseBody)
    accountResponse <- liftIO $ fetchUserInfo (_idpUri oauth2Info) accessToken

    let email = email_address (accountResponse ^. responseBody)
    let firstName = first_name (accountResponse ^. responseBody)
    let lastName = last_name (accountResponse ^. responseBody)

    account <- liftIO $ getOrCreateAccount conn email firstName lastName
    jwt <- liftIO $ getJwtToken account

    send $ withCookie "KvasirAuth" jwt $ redirect302 "/" 
    where
        getCode :: ResponderM TS.Text
        getCode = queryParam "code"
