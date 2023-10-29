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
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple (Connection, execute_, Query)

data AccountT f = Account
    { _accountId            :: Columnar f Text
    , _accountEmail         :: Columnar f Text
    , _accountFirstName     :: Columnar f Text
    , _accountLastName      :: Columnar f Text
    } deriving (Generic)

type Account = AccountT Identity
type AccountId = PrimaryKey AccountT Identity

deriving instance Show Account
deriving instance Eq Account

instance Beamable AccountT

instance Table AccountT where
    data PrimaryKey AccountT f = AccountId (Columnar f Text) deriving (Generic, Beamable)
    primaryKey = AccountId . _accountId

data PageT f = Page
    { _pageId               :: Columnar f Text
    , _pageTitle            :: Columnar f Text
    } deriving (Generic)

type Page = PageT Identity
type PageId = PrimaryKey PageT Identity

deriving instance Show Page
deriving instance Eq Page

instance Beamable PageT

instance Table PageT where
    data PrimaryKey PageT f = PageId (Columnar f Text) deriving (Generic, Beamable)
    primaryKey = PageId . _pageId

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

