{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE StandaloneDeriving #-}

module Database where

import Data.Functor
import Data.Int
import Data.Text (Text)
import Data.UUID as UUID (toText)
import Data.UUID.V4 as Uv4 (nextRandom)
import Database.Types (AccountT(Account), PageT(Page, _pageCreatorId), PrimaryKey (AccountId))
import Database.Beam
import Database.Beam.Query
import Database.Beam.Sqlite
import Database.SQLite.Simple (Connection, execute_, Query)

data KvasirDb f = KvasirDb
                { _kvasirAccounts   :: f (TableEntity AccountT)
                , _kvasirPages      :: f (TableEntity PageT)
                } deriving (Generic, Database be)

kvasirDb :: DatabaseSettings be KvasirDb
kvasirDb = defaultDbSettings `withDbModification`
            dbModification {
                _kvasirPages = modifyTableFields
                            tableModification {
                                _pageCreatorId = AccountId (fieldNamed "creator_id")
                            }
            }

createAccountTCmd :: Query
createAccountTCmd =
    "CREATE TABLE IF NOT EXISTS accounts ( \
        \id NVARCHAR(36) NOT NULL PRIMARY KEY, \
        \email NVARCHAR(254) NOT NULL, \
        \first_name NVARCHAR(64) NOT NULL, \
        \last_name NVARCHAR(64) NOT NULL\
    \);"

createPageTCmd :: Query
createPageTCmd =
    "CREATE TABLE IF NOT EXISTS pages ( \
        \id NVARCHAR(36) NOT NULL PRIMARY KEY, \
        \title NVARHCAR(128) NOT NULL, \
        \body TEXT NOT NULL, \
        \parent_id NVARCHAR(36) NULL, \
        \creator_id NVARCHAR(36) NOT NULL \
    \);"

ensureCreated :: Connection -> IO ()
ensureCreated conn = do
    execute_ conn createAccountTCmd
    execute_ conn createPageTCmd

createAccountIfNotExist :: Connection -> IO ()
createAccountIfNotExist conn = do
    accountCount <- runBeamSqliteDebug putStrLn conn
            $ runSelectReturningOne
            $ select
            $ aggregate_ (\a -> as_ @Int32 countAll_)
            $ all_ (_kvasirAccounts kvasirDb)

    case accountCount of
        Just 0 -> do
            accountId <- Uv4.nextRandom <&> UUID.toText
            _ <- runBeamSqliteDebug putStrLn conn
                $ runInsert
                $ insert (_kvasirAccounts kvasirDb)
                $ insertValues [ Account accountId "john.doe@citadel.local" "John" "Doe"
                               ]
            return ()
        _ -> return ()
    

seedKvasirDatabase :: Connection -> IO ()
seedKvasirDatabase conn = do
    ensureCreated conn
    -- Since we log in with OAuth2, we don't actually need to precreate an 
    -- account. I'll leave the line and the related statement for a bit though
    -- createAccountIfNotExist conn
    

