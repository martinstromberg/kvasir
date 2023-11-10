{-# LANGUAGE OverloadedStrings #-}

module Controllers.Pages where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Text as TS (Text)
import Data.Text.Lazy as TL (Text, fromStrict)
import Database.Page (getPageById)
import Database.Types (Account, Page, PageT(_pageTitle))
import Database.SQLite.Simple (Connection)
import Html.Types (Node)
import Utils
import Web.Twain
import Database.Account (getAccountById)
import Views.Pages (singlePageView)

handleGetPage :: Connection -> ResponderM a
handleGetPage conn = _getPage conn =<< isAuthenticated

_getPage :: Connection -> Bool -> ResponderM a
_getPage _ False = send $ redirect302 "/"
_getPage conn True = do
    accountId <- fromJust <$> authenticatedId
    account <- liftIO $ fromJust <$> getAccountById conn accountId
    pageId <- getPageId
    mPage <- liftIO $ getPage pageId conn

    getView mPage conn account
    where
        getPageId :: ResponderM (Maybe TS.Text)
        getPageId = paramMaybe "pageId"

        getPage :: Maybe TS.Text -> Connection -> IO (Maybe Page)
        getPage Nothing _ = return Nothing
        getPage (Just pageId) conn = getPageById pageId conn

        getPageTitle :: Page -> TL.Text
        getPageTitle = TL.fromStrict . _pageTitle

        getView :: Maybe Page -> Connection -> Account -> ResponderM a
        getView Nothing _ _ = send $ redirect302 "/"
        getView (Just page) conn acc = do
            respondWithHtmlNode conn (Just acc) (getPageTitle page)
            $ singlePageView page
            

