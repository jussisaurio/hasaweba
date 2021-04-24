{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns #-}

module Model where

import qualified Data.Text as T
import Database.SQLite.Simple.FromRow
import GHC.Generics (Generic)
import JSON (ToJSON, toJSON)

--- App resource definition types

class AppResource a

data User = User {userId :: Int, userName :: T.Text, userEmail :: T.Text} deriving (Eq, AppResource, Generic, ToJSON)

data Item = Item {itemId :: Int, itemDescription :: T.Text} deriving (Eq, AppResource, Generic, ToJSON)

-- Just to test that autoderiving our custom ToJSON instance works
data TestRecord = Eka {foo :: T.Text} | Toka {bar :: T.Text, user :: User, maybeOtherUser :: Maybe User, items :: [Item]} deriving (Eq, Generic, ToJSON)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance FromRow Item where
  fromRow = Item <$> field <*> field
