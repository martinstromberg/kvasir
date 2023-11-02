{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv
import Control.Monad.IO.Class (liftIO)
import Controllers
import qualified Data.Text as TS
import Data.List (isPrefixOf)
import qualified Data.Text as DT
import Database (seedKvasirDatabase)
import Database.SQLite.Simple (Connection, open)
import Network.Wai.Handler.Warp (run)
import System.Environment
import Types
import Web.Twain
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)

main :: IO ()
main = do
    putStrLn "Reading environment..."
    loadFile defaultConfig
    oauth2Info <- oauth2InfoFromEnv
    
    putStrLn "Connecting to database..."
    conn <- open "kvasir.db"
    seedKvasirDatabase conn

    port <- getEnv "KVASIR_PORT" >>= \s -> return (read s :: Int)
    putStrLn ("Starting Kvasir on port " <> show port)

    run port $ gzip def $ mkApp conn oauth2Info

mkApp :: Connection -> OAuth2Info -> Application
mkApp conn oauth2Info = foldr ($) serveStaticFiles $ routes conn oauth2Info

serveStaticFiles :: Application
serveStaticFiles =
    let
        ss = defaultWebAppSettings "./public"
    in staticApp ss

oauth2InfoFromEnv :: IO OAuth2Info
oauth2InfoFromEnv = do
    clientId <- getEnv "IDP_CLIENT_ID" >>= \s -> return $ TS.pack s
    clientSecret <- getEnv "IDP_CLIENT_SECRET" >>= \s -> return $ TS.pack s
    redirectUri <- getEnv "IDP_CLIENT_REDIRECT_URI" >>= \s -> return $ TS.pack s
    scope <- getEnv "IDP_CLIENT_SCOPE" >>= \s -> return $ TS.pack s
    idpUri <- getEnv "IDP_CLIENT_IDP_URI" >>= \s -> return $ TS.pack s

    return $ OAuth2Info
                { _clientId = clientId
                , _clientSecret = clientSecret
                , _redirectUri = redirectUri
                , _scope = scope
                , _idpUri = idpUri
                }

