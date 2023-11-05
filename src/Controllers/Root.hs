{-# LANGUAGE OverloadedStrings #-}
module Controllers.Root where

import Data.Text as TS
import Database.Account
import Database.Beam
import Database.SQLite.Simple (Connection)
import Database.Types
import Utils
import Views.Root
import Web.Twain

handleGetLandingPage :: Connection -> ResponderM a
handleGetLandingPage conn = do
    _getLandingPage conn =<< isAuthenticated

_getLandingPage :: Connection -> Bool -> ResponderM a
_getLandingPage conn True = do
    mId <- authenticatedId 
    mAcc <- liftIO $ getAccount mId

    case mAcc of
        Just acc -> do
            respondWithHtmlNode (Just acc) "Home"
            $ authenticatedLandingPage $ _accountFirstName acc
        Nothing -> do _getLandingPage conn False
    where
        getAccount :: Maybe TS.Text -> IO (Maybe (AccountT Identity))
        getAccount (Just accId) = do
            getAccountById conn accId
        getAccount _ = do return Nothing

_getLandingPage _ False = do
    respondWithHtmlNode Nothing "Home" guestLandingPage
