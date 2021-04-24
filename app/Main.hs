{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns #-}

module Main where

import AppContext
import Control.Monad.Except (throwError)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import JSON (ToJSON (..))
import Model
import Network.HTTP.Types
  ( status200,
    status404,
    status500,
  )
import Network.Wai
  ( Application,
    Request,
    Response,
    pathInfo,
    requestMethod,
    responseLBS,
  )
import Network.Wai.Handler.Warp (run)

cfg :: Config
cfg = Config {db = "hasa.db"}

env :: Env
env = Env {config = cfg}

data Route = GetUser T.Text | GetUsers | GetItem T.Text | GetItems deriving (Show)

initDB :: String -> IO ()
initDB dbname = do
  conn <- SQLite.open dbname
  SQLite.execute_ conn "CREATE TABLE IF NOT EXISTS users (id integer primary key, name text, email text unique)"
  SQLite.execute_ conn "CREATE TABLE IF NOT EXISTS items (id integer primary key, description text)"
  SQLite.execute_ conn "DELETE FROM users"
  SQLite.execute_ conn "DELETE FROM items"
  SQLite.execute_ conn "INSERT INTO users (name, email) VALUES ('Jussi Saurio', 'jussi.saurio@reaktor.com'), ('Other Dude', 'other@dudeson.com')"
  SQLite.execute_ conn "INSERT INTO items (description) VALUES ('Cookies'), ('Cream')"

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead lst = Just $ head lst

main :: IO ()
main = do
  initDB (db $ config env)
  run 1337 $ app env

-- From WAI:
-- Application is a type alias for: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
-- Callback is a type alias for: (Response -> IO ResponseReceived)
app :: Env -> Application
app env' request callback = runApp (router request >>= handler) env' >>= callback . handleError

respondJSON :: (ToJSON a) => a -> AppCtx Response
respondJSON a = pure (responseLBS status200 [] (LB.pack $ toJSON a))

handler :: Route -> AppCtx Response
handler route = case route of
  GetUsers -> do
    users <- runDB "SELECT * from users" Nothing :: AppCtx [User]
    respondJSON users
  GetUser uid -> do
    maybeUser <- safeHead <$> runDB "SELECT * from users WHERE id = ?" (Just [uid]) :: AppCtx (Maybe User)
    case maybeUser of
      Nothing -> throwError (Error404 "not found")
      Just user -> respondJSON user
  GetItems -> do
    items <- runDB "SELECT * from items" Nothing :: AppCtx [Item]
    respondJSON items
  GetItem itid -> do
    maybeItem <- safeHead <$> runDB "SELECT * from items WHERE id = ?" (Just [itid]) :: AppCtx (Maybe Item)
    case maybeItem of
      Nothing -> throwError (Error404 "not found")
      Just item -> respondJSON item

handleError :: Either Error Response -> Response
handleError = either _handle id
  where
    _handle = \case
      Error404 msg -> responseLBS status404 [] msg
      Error500 _ -> responseLBS status500 [] "Something went wrong"

router :: Request -> AppCtx Route
router rq =
  liftEither $ case (pathInfo rq, requestMethod rq) of
    (["users", uid], "GET") -> Right (GetUser uid)
    (["users"], "GET") -> Right GetUsers
    (["items", itid], "GET") -> Right (GetItem itid)
    (["items"], "GET") -> Right GetItems
    _ -> Left $ Error404 "Not found"