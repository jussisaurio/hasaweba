{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns #-}

module Model where

import qualified Data.Text as T
import Database.SQLite.Simple.FromRow
import GHC.Generics
import JSON

--- App resource definition types

class AppResource a

data User = User {userId :: Int, userName :: T.Text, userEmail :: T.Text} deriving (Eq, Show, AppResource, Generic, ToJSON, FromJSON)

data Item = Item {itemId :: Int, itemDescription :: T.Text} deriving (Eq, Show, AppResource, Generic, ToJSON, FromJSON)

-- Just to test that autoderiving our custom ToJSON and FromJSON instances works
data TestRecord = Eka {foo :: T.Text} | Toka {bar :: T.Text, user :: User, maybeOtherUser :: Maybe User, items :: [Item]} deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance FromRow Item where
  fromRow = Item <$> field <*> field
