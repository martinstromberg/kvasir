{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database (seedKvasirDatabase)
import Database.SQLite.Simple (Connection, open)
import Handlers
import Network.Wai.Handler.Warp (run)
import Web.Twain

main :: IO ()
main = do
    putStrLn "Connecting to database..."
    conn <- open "kvasir.db"
    seedKvasirDatabase conn

    putStrLn "Starting Kvasir on port 9099"
    run 9099 $ foldr ($) (notFound missing) (routes conn)
