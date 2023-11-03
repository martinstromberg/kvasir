module Database.Account where

import Data.Functor
import Data.Maybe
import Data.Text as TS (Text)
import Data.UUID as UUID (toText)
import Data.UUID.V4 as Uv4 (nextRandom)
import Database (KvasirDb(_kvasirAccounts), kvasirDb)
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple (Connection)
import Database.Types (AccountT(Account, _accountEmail))

getAccountByEmailAddress :: Connection -> TS.Text -> IO (Maybe (AccountT Identity))
getAccountByEmailAddress conn email = do
    runBeamSqlite conn
        $ runSelectReturningOne
        $ select
        $ filter_ (\s -> _accountEmail s ==. val_ email)
        $ all_ (_kvasirAccounts kvasirDb)

createAccount :: Connection -> Text -> Text -> Text -> IO (AccountT Identity)
createAccount conn email fn ln = do
    accountId <- Uv4.nextRandom <&> UUID.toText

    let account = Account accountId email fn ln

    runBeamSqlite conn
        $ runInsert
        $ insert (_kvasirAccounts kvasirDb)
        $ insertValues [account]

    return account

getOrCreateAccount :: Connection -> Text -> Text -> Text -> IO (AccountT Identity)
getOrCreateAccount conn email fn ln = do
    m <- getAccountByEmailAddress conn email

    case m of
        Just acc -> return acc
        Nothing -> createAccount conn email fn ln

