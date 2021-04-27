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
import Data.Char (toLower, toUpper)
import Data.List
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import GHC.Generics
import Parser
import Text.Read (readMaybe)

-- Serializer
class ToJSON (a :: *) where
  toJSON :: a -> JSON
  default toJSON :: (Generic a, GenericToJSON (Rep a)) => a -> JSON
  toJSON a = genericToJSON (from a)

class GenericToJSON (f :: * -> *) where
  genericToJSON :: f a -> JSON

instance ToJSON Int where
  toJSON = JSONNumber . show

instance ToJSON T.Text where
  toJSON = JSONString . T.unpack

instance (ToJSON a) => ToJSON (Maybe a) where
  toJSON Nothing = JSONNull
  toJSON (Just x) = toJSON x

instance (ToJSON a) => ToJSON [a] where
  toJSON = JSONArray . map toJSON

instance (ToJSON recordValue) => GenericToJSON (K1 i recordValue) where
  genericToJSON x = let recordValue = unK1 x in toJSON recordValue

instance (Selector s, GenericToJSON recordValue) => GenericToJSON (M1 S s recordValue) where
  genericToJSON x =
    let key = selName (undefined :: M1 S s recordValue a)
        value = genericToJSON (unM1 x)
     in JSONObject [(JSONKey key, value)]

instance (GenericToJSON keyValueLeft, GenericToJSON keyValueRight) => GenericToJSON (keyValueLeft :*: keyValueRight) where
  genericToJSON (x :*: y) =
    let (JSONObject entries1) = genericToJSON x
        (JSONObject entries2) = genericToJSON y
     in JSONObject $ entries1 ++ entries2

instance (Constructor c, GenericToJSON fields) => GenericToJSON (M1 C c fields) where
  genericToJSON x =
    let json = genericToJSON $ unM1 x
        ownName = conName (undefined :: M1 C c fields any)
        removeOwnName (JSONKey k, v) = let key = if length ownName < length k && map toLower ownName `isPrefixOf` k then camelify $ drop (length ownName) k else k in (JSONKey key, v)
        camelify "" = ""
        camelify (x : xs) = toLower x : xs
     in case json of
          (JSONObject entries) -> JSONObject (map removeOwnName entries)
          other -> other

instance (GenericToJSON constructorLeft, GenericToJSON constructorRight) => GenericToJSON (constructorLeft :+: constructorRight) where
  genericToJSON (L1 x) = genericToJSON x
  genericToJSON (R1 x) = genericToJSON x

instance GenericToJSON constructors => GenericToJSON (M1 D d constructors) where
  genericToJSON = genericToJSON . unM1

-- JSON Tokenizer

data JSONToken = ObjectStart | ObjectEnd | ArrayStart | ArrayEnd | Colon | Comma | JString String | JNumber String | JNull | JTrue | JFalse deriving (Eq, Show)

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
stringToken = Just . JString . concat <$> (prefix "\"" *> zeroOrMore (prefix "\\\"" <|> notPrefix "\"") <* prefix "\"")

numberToken :: Parser Char (Maybe JSONToken)
numberToken = Just . JNumber <$> ((++) <$> intParser <*> (concat <$> maybeOne ((++) <$> prefix "." <*> posIntParser)))

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

data JSON = JSONString String | JSONNumber String | JSONBoolean Bool | JSONNull | JSONArray [JSON] | JSONObject [(JSONKey, JSON)] deriving (Eq, Show)

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

jsonSerialize :: JSON -> String
jsonSerialize = \case
  JSONNull -> "null"
  (JSONBoolean True) -> "true"
  (JSONBoolean False) -> "false"
  (JSONString s) -> show s
  (JSONNumber n) -> n
  (JSONArray jsons) -> "[" <> intercalate "," (map jsonSerialize jsons) <> "]"
  (JSONObject entries) -> "{" <> intercalate "," (map (\(JSONKey k, v) -> "\"" <> k <> "\":" <> jsonSerialize v) entries) <> "}"

-- Generic JSON to Record parser

type JSONError = String

class FromJSON (a :: *) where
  fromJSON :: JSON -> Either JSONError a
  default fromJSON :: (Generic a, GenericFromJSON (Rep a)) => JSON -> Either JSONError a
  fromJSON = fmap to . genericFromJSON

class GenericFromJSON (f :: * -> *) where
  genericFromJSON :: JSON -> Either JSONError (f a)
  withConName :: String -> JSON -> Either JSONError (f a)
  withConName cname = \case
    (JSONObject entries) -> genericFromJSON $ JSONObject (entries ++ mapped)
      where
        mapped = map (\(JSONKey k, v) -> (JSONKey (map toLower cname ++ capitalize k), v)) entries
        capitalize "" = ""
        capitalize (x : xs) = toUpper x : xs

instance FromJSON Int where
  fromJSON = \case
    (JSONNumber n) -> case readMaybe n of
      Nothing -> Left "Failed to parse int"
      (Just n) -> if n == fromInteger (round n) then Right (round n) else Left "failed to parse Int, got a non-integer number"
    other -> Left $ "Failed to parse Int, got " <> show other

instance FromJSON Double where
  fromJSON = \case
    (JSONNumber n) -> case readMaybe n of
      Nothing -> Left "Failed to parse double"
      (Just n) -> Right n
    other -> Left $ "Failed to parse Int, got " <> show other

instance FromJSON T.Text where
  fromJSON json = case json of
    (JSONString s) -> Right $ T.pack s
    other -> Left $ "Failed to parse Text, got " <> show other

instance FromJSON String where
  fromJSON json = case json of
    (JSONString s) -> Right s
    other -> Left $ "Failed to parse Text, got " <> show other

instance FromJSON a => FromJSON (Maybe a) where
  fromJSON = \case
    JSONNull -> Right Nothing
    other -> fromJSON other

instance FromJSON a => FromJSON [a] where
  fromJSON = \case
    JSONArray json -> traverse fromJSON json
    other -> Left $ "expected array, got " <> show other

instance (FromJSON recordValue) => GenericFromJSON (K1 i recordValue) where
  genericFromJSON = fmap K1 . fromJSON

instance (Selector s, GenericFromJSON recordValue) => GenericFromJSON (M1 S s recordValue) where
  genericFromJSON json =
    let key = selName (undefined :: M1 S s recordValue a)
     in case json of
          (JSONObject entries) -> case snd <$> find ((== JSONKey key) . fst) entries of
            Nothing -> Left "failed to parse selector, no matching key"
            Just v -> M1 <$> genericFromJSON v
          other -> Left $ "failed to parse selector, got wrong type of JSON: " <> show other

instance (GenericFromJSON keyValueLeft, GenericFromJSON keyValueRight) => GenericFromJSON (keyValueLeft :*: keyValueRight) where
  genericFromJSON json = do
    r1 <- genericFromJSON json :: Either JSONError (keyValueLeft a)
    r2 <- genericFromJSON json :: Either JSONError (keyValueRight b)
    pure (r1 :*: r2)

instance (Constructor c, GenericFromJSON fields) => GenericFromJSON (M1 C c fields) where
  genericFromJSON =
    let cname = conName (undefined :: M1 C c fields a) in fmap M1 . withConName cname

instance (GenericFromJSON constructorLeft, GenericFromJSON constructorRight) => GenericFromJSON (constructorLeft :+: constructorRight) where
  genericFromJSON json =
    let e1 = genericFromJSON json :: Either JSONError (constructorLeft a)
        e2 = genericFromJSON json :: Either JSONError (constructorRight b)
     in case (e1, e2) of
          (Right x, Left _) -> Right $ L1 x
          (Left _, Right x) -> Right $ R1 x
          (Right _, Right _) -> Left "Failed to parse constructor, both branches returned Right?"
          (Left er, Left er2) -> Left $ "Failed to parse constructor, both branches returned Left: " <> er <> " " <> er2

instance GenericFromJSON constructors => GenericFromJSON (M1 D d constructors) where
  genericFromJSON = fmap M1 . genericFromJSON