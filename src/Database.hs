{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database
    ( seedKvasirDatabase
    ) where

import Data.Functor
import Data.Text (Text)
import Data.UUID as UUID (toText)
import Data.UUID.V4 as Uv4 (nextRandom)
import Database.Account (AccountT(Account))
import Database.Beam
import Database.Beam.Sqlite
import Database.Page (PageT(Page))
import Database.SQLite.Simple (Connection, execute_, Query)

data KvasirDb f = KvasirDb
                { _kvasirAccounts   :: f (TableEntity AccountT)
                , _kvasirPages      :: f (TableEntity PageT)
                } deriving (Generic, Database be)

kvasirDb :: DatabaseSettings be KvasirDb
kvasirDb = defaultDbSettings

createAccountTCmd :: Query
createAccountTCmd = "CREATE TABLE IF NOT EXISTS accounts (id NVARCHAR(36) NOT NULL PRIMARY KEY, email NVARCHAR(254) NOT NULL, first_name NVARCHAR(64) NOT NULL, last_name NVARCHAR(64) NOT NULL);"

ensureCreated :: Connection -> IO ()
ensureCreated conn = do
    execute_ conn createAccountTCmd

seedKvasirDatabase :: Connection -> IO ()
seedKvasirDatabase conn = do
    ensureCreated conn

    accountId <- Uv4.nextRandom <&> UUID.toText
    runBeamSqliteDebug putStrLn conn
        $ runInsert
        $ insert (_kvasirAccounts kvasirDb)
        $ insertValues [ Account accountId "john.doe@citadel.local" "John" "Doe"
                       ]

