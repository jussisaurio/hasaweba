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
import Data.List (intercalate)
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

class GetFields x where
  getFields :: x -> [String]

class GetValues x where
  getValues :: x -> [String]

instance (GetFields (a p), GetFields (b p)) => GetFields ((a :*: b) p) where
  getFields (x :*: y) = getFields x ++ getFields y

instance Selector s => GetFields (M1 S s f a) where
  getFields x = [quoteWrap $ selName x]

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

instance ToJSON User where
  toJSON user = let rep = unM1 . unM1 . G.from in helper (getFields $ rep user) (getValues $ rep user)

instance ToJSON Item where
  toJSON item = let rep = unM1 . unM1 . G.from in helper (getFields $ rep item) (getValues $ rep item)

instance (ToJSON a) => ToJSON [a] where
  toJSON [] = array ""
  toJSON lst = array $ LB.intercalate "," (map toJSON lst)