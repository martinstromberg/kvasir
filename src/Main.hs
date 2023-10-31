{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Controllers
import qualified Data.Text as DT
import Data.List (isPrefixOf)
import Database (seedKvasirDatabase)
import Database.SQLite.Simple (Connection, open)
import Network.Wai.Handler.Warp (run)
import Web.Twain
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)

main :: IO ()
main = do
    putStrLn "Connecting to database..."
    conn <- open "kvasir.db"
    seedKvasirDatabase conn

    putStrLn "Starting Kvasir on port 9099"
    run 9099 $ gzip def $ mkApp conn

mkApp :: Connection -> Application
mkApp conn = foldr ($) serveStaticFiles (routes conn)

serveStaticFiles :: Application
serveStaticFiles =
    let
        ss = defaultWebAppSettings "./public"
    in staticApp ss

