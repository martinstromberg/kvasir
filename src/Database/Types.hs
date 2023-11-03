{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Types where

import Data.Text (Text)
import Database.Beam

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

