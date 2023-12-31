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
    } deriving (Generic, Beamable)
type Account = AccountT Identity

deriving instance Show Account
deriving instance Show (PrimaryKey AccountT Identity)
deriving instance Eq Account

instance Table AccountT where
    data PrimaryKey AccountT f = AccountId (Columnar f Text) deriving (Generic, Beamable)
    primaryKey = AccountId . _accountId
type AccountId = PrimaryKey AccountT Identity


data PageT f = Page
    { _pageId                   :: Columnar f Text
    , _pageTitle                :: Columnar f Text
    , _pageBody                 :: Columnar f Text
    , _pageParentId             :: Columnar f (Maybe Text)
    , _pageCreatorId            :: PrimaryKey AccountT f
    } deriving (Generic, Beamable)
type Page = PageT Identity

deriving instance Show Page
deriving instance Show (PrimaryKey PageT Identity)

instance Table PageT where
    data PrimaryKey PageT f = PageId (Columnar f Text) deriving (Generic, Beamable)
    primaryKey = PageId . _pageId
type PageId = PrimaryKey PageT Identity

