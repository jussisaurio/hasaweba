{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns #-}

module Model where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (toLower)
import Data.List (intercalate, isPrefixOf)
import Data.String (IsString)
import qualified Data.Text as T
import Database.SQLite.Simple.FromRow
import GHC.Generics
import qualified GHC.Generics as G

-- Let's write a JSON serializer from scratch because why not

helper :: [[Char]] -> [[Char]] -> LB.ByteString
helper keys values = LB.pack . object . intercalate "," $ zipWith (\k v -> k <> ":" <> v) keys values

class (G.Generic a) => ToJSON a where
  toJSON :: a -> LBS.ByteString

class GetOwnName x where
  getOwnName :: x -> String

instance Constructor c => GetOwnName (M1 C c d e) where
  getOwnName = conName

class GetFields x where
  getFields :: x -> [String]

class GetValues x where
  getValues :: x -> [String]

instance (GetFields (a p), GetFields (b p)) => GetFields ((a :*: b) p) where
  getFields (x :*: y) = getFields x ++ getFields y

instance Selector s => GetFields (M1 S s f a) where
  getFields x = [selName x]

instance (GetValues (a p), GetValues (b p)) => GetValues ((a :*: b) p) where
  getValues (x :*: y) = getValues x ++ getValues y

instance Show b => GetValues (M1 d e (K1 a b) f) where
  getValues x = [show $ unK1 $ unM1 x]

object :: (Semigroup a, IsString a) => a -> a
object str = "{" <> str <> "}"

array :: (Semigroup a, IsString a) => a -> a
array str = "[" <> str <> "]"

quoteWrap :: (Semigroup a, IsString a) => a -> a
quoteWrap str = "\"" <> str <> "\""

--- App resource definition types

class AppResource a

data User = User {userId :: Int, userName :: T.Text, userEmail :: T.Text} deriving (Eq, G.Generic, AppResource)

data Item = Item {itemId :: Int, itemDescription :: T.Text} deriving (Eq, G.Generic, AppResource)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance FromRow Item where
  fromRow = Item <$> field <*> field

defaultToJson :: (GetFields (f p), GetValues (f p), GetOwnName (M1 i1 c1 f p)) => M1 i2 c2 (M1 i1 c1 f) p -> LB.ByteString
defaultToJson repre =
  let meta = unM1 repre
      rep = unM1 meta
      ownName = map toLower . getOwnName $ meta
      stripOwnName f = if ownName `isPrefixOf` f then drop (length ownName) f else f
      buildObject keys values = LB.pack . object . intercalate "," $ zipWith (\k v -> k <> ":" <> v) keys values
   in buildObject (map (map toLower . quoteWrap . stripOwnName) . getFields $ rep) (getValues $ rep)

instance ToJSON User where
  toJSON = defaultToJson . G.from

instance ToJSON Item where
  toJSON = defaultToJson . G.from

instance (ToJSON a) => ToJSON [a] where
  toJSON [] = array ""
  toJSON lst = array $ LB.intercalate "," (map toJSON lst)