module Routes where

import AppContext
import Control.Monad.Except
import FancyRouting
import Model

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead lst = Just $ head lst

getUser :: Int -> AppCtx User
getUser uid = do
  maybeUser <- safeHead <$> runDB "SELECT * from users WHERE id = ?" (Just [uid]) :: AppCtx (Maybe User)
  case maybeUser of
    Nothing -> throwError (Error404 "not found")
    Just user' -> pure user'

getUsers :: AppCtx [User]
getUsers = runDB "SELECT * from users" Nothing :: AppCtx [User]

getItem :: Int -> AppCtx Item
getItem itid = do
  maybeItem <- safeHead <$> runDB "SELECT * from items WHERE id = ?" (Just [itid]) :: AppCtx (Maybe Item)
  case maybeItem of
    Nothing -> throwError (Error404 "not found")
    Just item -> pure item

getItems :: AppCtx [Item]
getItems = runDB "SELECT * from items" Nothing :: AppCtx [Item]

type GetUser = "users" :> Capture Int :> Get User

type GetUsers = "users" :> Get [User]

type GetItem = "items" :> Capture Int :> Get Item

type GetItems = "items" :> Get [Item]

type API = GetUser :<|> GetUsers :<|> GetItem :<|> GetItems

api :: Server API
api = getUser :<|> getUsers :<|> getItem :<|> getItems