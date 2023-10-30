{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Page where

import Data.Text (Text)
import Database.Beam

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

