module Database.Page where

import Data.Functor
import Data.Maybe
import Data.Text as TS (Text)
import Data.UUID as UUID (toText)
import Data.UUID.V4 as Uv4 (nextRandom)
import Database (KvasirDb(_kvasirAccounts, _kvasirPages), kvasirDb)
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple (Connection)
import Database.Types

getTopPagesByCreator :: TS.Text -> Connection -> IO [PageT Identity]
getTopPagesByCreator accountId conn =
    -- I wish I could find a way to do this without joining but beam man, idk...
    runBeamSqlite conn $ runSelectReturningList $ select $ do
        accounts <- all_ (_kvasirAccounts kvasirDb)
        pages <- oneToMany_ (_kvasirPages kvasirDb) _pageCreatorId accounts
        guard_ (_accountId accounts ==. val_ accountId)
        pure pages

getPageById :: TS.Text -> Connection -> IO (Maybe (PageT Identity))
getPageById pageId conn =
    runBeamSqlite conn
    $ runSelectReturningOne
    $ select
    $ filter_ (\p -> _pageId p  ==. val_ pageId)
    $ all_ (_kvasirPages kvasirDb)
