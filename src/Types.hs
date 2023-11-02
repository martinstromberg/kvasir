module Types where

import Data.Text as TS

data OAuth2Info = OAuth2Info
    { _clientId         :: TS.Text
    , _clientSecret     :: TS.Text
    , _redirectUri      :: TS.Text
    , _scope             :: TS.Text
    , _idpUri            :: TS.Text
    } deriving (Show)

