module Database.Page where

import Data.Functor
import Data.Maybe
import Data.Text as TS (Text)
import Data.UUID as UUID (toText)
import Data.UUID.V4 as Uv4 (nextRandom)
import Database (KvasirDb(_kvasirAccounts, _kvasirPages), kvasirDb)
import Database.Beam
import Database.Beam.Query
import Database.Beam.Sqlite
import Database.SQLite.Simple (Connection)
import Database.Types

getTopPagesByCreator :: TS.Text -> Connection -> IO [PageT Identity]
getTopPagesByCreator accountId conn = do
    runBeamSqlite conn
        $ runSelectReturningList
        $ select
        $ filter_ (\s -> _pageCreatorId s ==. val_ accountId)
        $ all_ (_kvasirPages kvasirDb)

