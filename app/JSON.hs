{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module JSON where

import Control.Applicative (Alternative ((<|>)))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import GHC.Generics
import Parser

-- Serializer
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

instance (ToJSON recordValue) => GenericToJSON (K1 i recordValue) where
  genericToJSON x = let recordValue = unK1 x in toJSON recordValue

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

-- JSON Tokenizer

data JSONToken = ObjectStart | ObjectEnd | ArrayStart | ArrayEnd | Colon | Comma | JString String | JNumber Double | JNull | JTrue | JFalse deriving (Eq, Show)

newline :: Parser Char (Maybe a)
newline = Nothing <$ (prefix "\r\n" <|> prefix "\n")

jspace :: Parser Char (Maybe a)
jspace = Nothing <$ space

objectStart :: Parser Char (Maybe JSONToken)
objectStart = Just ObjectStart <$ char '{'

objectEnd :: Parser Char (Maybe JSONToken)
objectEnd = Just ObjectEnd <$ char '}'

arrayStart :: Parser Char (Maybe JSONToken)
arrayStart = Just ArrayStart <$ char '['

arrayEnd :: Parser Char (Maybe JSONToken)
arrayEnd = Just ArrayEnd <$ char ']'

colon :: Parser Char (Maybe JSONToken)
colon = Just Colon <$ char ':'

comma :: Parser Char (Maybe JSONToken)
comma = Just Comma <$ char ','

stringToken :: Parser Char (Maybe JSONToken)
stringToken = Just . JString <$> (prefix "\"" *> zeroOrMore (anythingBut '"') <* prefix "\"")

numberToken :: Parser Char (Maybe JSONToken)
numberToken = Just . JNumber . read <$> ((++) <$> intParser <*> (concat <$> maybeOne ((++) <$> prefix "." <*> posIntParser)))

jnull :: Parser Char (Maybe JSONToken)
jnull = Just JNull <$ prefix "null"

jbool :: Parser Char (Maybe JSONToken)
jbool = Just JTrue <$ prefix "true" <|> Just JFalse <$ prefix "false"

tokenizer :: Parser Char [Maybe JSONToken]
tokenizer = oneOrMore (newline <|> jspace <|> objectStart <|> objectEnd <|> arrayStart <|> arrayEnd <|> colon <|> comma <|> stringToken <|> numberToken <|> jnull <|> jbool)

tokenize :: [Char] -> Either String [JSONToken]
tokenize input = case runParser tokenizer input of
  Nothing -> Left "tokenizer error"
  Just (tokenMaybes, "") -> Right (catMaybes tokenMaybes)
  Just (_, remainder) -> Left ("tokenizer error: remainder is " <> remainder)

-- JSON Parser

newtype JSONKey = JSONKey String deriving (Eq, Show)

data JSON = JSONString String | JSONNumber Double | JSONBoolean Bool | JSONNull | JSONArray [JSON] | JSONObject [(JSONKey, JSON)] deriving (Eq, Show)

parseNull :: Parser JSONToken JSON
parseNull = JSONNull <$ satisfy (JNull ==)

parseBool :: Parser JSONToken JSON
parseBool = JSONBoolean True <$ satisfy (JTrue ==) <|> JSONBoolean False <$ satisfy (JFalse ==)

parseString :: Parser JSONToken JSON
parseString =
  JSONString . (\(JString x) -> x)
    <$> satisfy
      ( \case
          JString _ -> True
          _ -> False
      )

parseNumber :: Parser JSONToken JSON
parseNumber =
  JSONNumber . (\(JNumber x) -> x)
    <$> satisfy
      ( \case
          JNumber _ -> True
          _ -> False
      )

parseKey :: Parser JSONToken JSONKey
parseKey =
  JSONKey . (\(JString x) -> x)
    <$> satisfy
      ( \case
          JString _ -> True
          _ -> False
      )
      <* satisfy (== Colon)

parseLiteral :: Parser JSONToken JSON
parseLiteral = parseNull <|> parseBool <|> parseString <|> parseNumber

parseJSON :: Parser JSONToken JSON
parseJSON = parseLiteral <|> parseJSONArray <|> parseObject

parseArray :: Parser JSONToken [JSON]
parseArray = (++) <$> zeroOrMore (parseJSON <* satisfy (== Comma)) <*> maybeOne parseJSON

parseJSONArray :: Parser JSONToken JSON
parseJSONArray = JSONArray <$> (satisfy (== ArrayStart) *> parseArray <* satisfy (== ArrayEnd))

parseKVPair :: Parser JSONToken (JSONKey, JSON)
parseKVPair = (,) <$> parseKey <*> parseJSON

parseEntries :: Parser JSONToken [(JSONKey, JSON)]
parseEntries = (++) <$> zeroOrMore (parseKVPair <* satisfy (== Comma)) <*> maybeOne parseKVPair

parseObject :: Parser JSONToken JSON
parseObject = JSONObject <$> (satisfy (== ObjectStart) *> parseEntries <* satisfy (== ObjectEnd))

jsonParse :: [JSONToken] -> Either String JSON
jsonParse tokens = case runParser parseJSON tokens of
  Nothing -> Left "parse error"
  Just (json, []) -> Right json
  Just (_, remainder) -> Left ("parse error: remainder is " <> show remainder)