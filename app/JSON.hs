{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module JSON where

import Data.List (intercalate)
import qualified Data.Text as T
import GHC.Generics

class ToJSON (a :: *) where
  toJSON :: a -> String
  default toJSON :: (Generic a, GenericToJSON (Rep a)) => a -> String
  toJSON a = genericToJSON (from a)

class GenericToJSON (f :: * -> *) where
  genericToJSON :: f a -> String

instance ToJSON Int where
  toJSON = show

instance ToJSON T.Text where
  toJSON = show

instance (ToJSON a) => ToJSON (Maybe a) where
  toJSON Nothing = "null"
  toJSON (Just x) = toJSON x

instance (ToJSON a) => ToJSON [a] where
  toJSON lst = "[" <> (intercalate "," . map toJSON $ lst) <> "]"

instance (ToJSON a) => GenericToJSON (K1 i a) where
  genericToJSON x = let jsonable = unK1 x in toJSON jsonable

instance (Selector s, GenericToJSON recordValue) => GenericToJSON (M1 S s recordValue) where
  genericToJSON x =
    let key = selName (undefined :: M1 S s recordValue a)
        value = genericToJSON (unM1 x)
     in "\"" <> key <> "\":" <> value

instance (GenericToJSON keyValueLeft, GenericToJSON keyValueRight) => GenericToJSON (keyValueLeft :*: keyValueRight) where
  genericToJSON (x :*: y) = genericToJSON x <> "," <> genericToJSON y

instance (Constructor c, GenericToJSON fields) => GenericToJSON (M1 C c fields) where
  genericToJSON x = let fields = unM1 x in "{" <> genericToJSON fields <> "}"

instance (GenericToJSON constructorLeft, GenericToJSON constructorRight) => GenericToJSON (constructorLeft :+: constructorRight) where
  genericToJSON (L1 x) = genericToJSON x
  genericToJSON (R1 x) = genericToJSON x

instance GenericToJSON constructors => GenericToJSON (M1 D d constructors) where
  genericToJSON x = let constructors = unM1 x in genericToJSON constructors
