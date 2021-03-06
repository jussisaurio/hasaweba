module Main where

import AppContext (Config (..), Env (..), Error (..), runApp)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Proxy (Proxy (..))
import qualified Database.SQLite.Simple as SQLite
import FancyRouting (serve)
import Network.HTTP.Types
  ( status200,
    status400,
    status404,
    status500,
  )
import Network.Wai
  ( Application,
    Response,
    responseLBS,
  )
import Network.Wai.Handler.Warp (run)
import Routes (API, api)

cfg :: Config
cfg = Config {db = "hasa.db"}

env :: Env
env = Env {config = cfg}

initDB :: String -> IO ()
initDB dbname = do
  conn <- SQLite.open dbname
  SQLite.execute_ conn "CREATE TABLE IF NOT EXISTS users (id integer primary key, name text, email text unique)"
  SQLite.execute_ conn "CREATE TABLE IF NOT EXISTS items (id integer primary key, description text)"
  SQLite.execute_ conn "DELETE FROM users"
  SQLite.execute_ conn "DELETE FROM items"
  SQLite.execute_ conn "INSERT INTO users (name, email) VALUES ('Jussi Saurio', 'jussi.saurio@reaktor.com'), ('Other Dude', 'other@dudeson.com')"
  SQLite.execute_ conn "INSERT INTO items (description) VALUES ('Cookies'), ('Cream')"

main :: IO ()
main = do
  initDB (db $ config env)
  run 1337 $ app env

proxy :: Proxy API
proxy = Proxy

app :: Env -> Application
app env' request callback = runApp (serve proxy api request) env' >>= callback . handle

handle :: Either Error LB.ByteString -> Response
handle = either handleError handleSuccess
  where
    handleError = \case
      Error404 msg -> responseLBS status404 [] msg
      Error400 msg -> responseLBS status400 [] msg
      Error500 _ -> responseLBS status500 [] "Something went wrong"
    handleSuccess = responseLBS status200 []