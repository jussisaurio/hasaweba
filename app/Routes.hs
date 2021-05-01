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

validateUser :: Int -> User -> AppCtx User
validateUser uid user' = do
  dbUser <- getUser uid
  if dbUser == user' then pure user' else throwError (Error400 "Not the same user")

type GetUser = GET :> "users" :> Capture Int :> Respond User

type GetUsers = GET :> "users" :> Respond [User]

type ValidateUser = POST :> "users" :> Capture Int :> "validate" :> BodyParser User :> Respond User

type GetItem = GET :> "items" :> Capture Int :> Respond Item

type GetItems = GET :> "items" :> Respond [Item]

type API = GetUser :<|> GetUsers :<|> ValidateUser :<|> GetItem :<|> GetItems

api :: Server API
api = getUser :<|> getUsers :<|> validateUser :<|> getItem :<|> getItems