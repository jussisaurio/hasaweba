{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import AppContext
  ( AppContext,
    AppCtx,
    Config (Config, db, hardcodedResponse),
    Env (..),
    Error (..),
    getConfig,
    liftEither,
    runApp,
  )
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
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

cfg = Config {db = "todo", hardcodedResponse = "hello world!"}

env = Env {config = cfg}

data Route = GetHello (Maybe T.Text)

main :: IO ()
main = run 1337 $ app env

-- From WAI:
-- Application is a type alias for: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
-- Callback is a type alias for: (Response -> IO ResponseReceived)
app :: Env -> Application
app env request callback = runApp (router request >>= handler) env >>= callback . handleError

handler :: Route -> AppCtx Response
handler route = do
  config <- getConfig
  let greeting = case route of
        GetHello Nothing -> hardcodedResponse config
        GetHello (Just t) -> encodeUtf8 . fromStrict $ T.concat ["hello ", t, "!"]
  return $ responseLBS status200 [] greeting

handleError :: Either Error Response -> Response
handleError = either _handle id
  where
    _handle error = case error of
      Error404 msg -> responseLBS status404 [] msg
      Error500 _ -> responseLBS status500 [] "Something went wrong"

router :: Request -> AppCtx Route
router rq =
  liftEither $ case (pathInfo rq, requestMethod rq) of
    (["hello", id], "GET") -> Right (GetHello $ Just id)
    (["hello"], "GET") -> Right (GetHello Nothing)
    _ -> Left $ Error404 "Not found"