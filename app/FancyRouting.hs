module FancyRouting where

import AppContext
import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Except
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.TypeLits
import JSON
import Network.Wai
  ( Request,
    pathInfo,
    requestMethod,
  )
import Text.Read

-- Adapted (stolen) from https://www.well-typed.com/blog/2015/11/implementing-a-minimal-version-of-haskell-servant/
data Respond (a :: *)

data GET

data POST

data a :<|> b = a :<|> b

infixr 8 :<|>

data (a :: k) :> (b :: *)

infixr 9 :>

data Capture (a :: *)

type family Server layout :: *

type instance Server (Respond a) = AppCtx a

type instance Server (a :<|> b) = Server a :<|> Server b

type instance Server ((s :: Symbol) :> r) = Server r

type instance Server (GET :> r) = Server r

type instance Server (POST :> r) = Server r

type instance Server (Capture a :> r) = a -> Server r

class HasServer layout a where
  route :: Proxy layout -> Server layout -> (T.Text, [T.Text]) -> Maybe (AppCtx a)

instance (ToJSON a) => HasServer (Respond a) JSON where
  route _ handler (_, []) = Just (toJSON <$> handler)
  route _ _ _ = Nothing

instance (HasServer a x, HasServer b x) => HasServer (a :<|> b) x where
  route :: Proxy (a :<|> b) -> (Server a :<|> Server b) -> (T.Text, [T.Text]) -> Maybe (AppCtx x)
  route _ (handlera :<|> handlerb) req = route (Proxy :: Proxy a) handlera req <|> route (Proxy :: Proxy b) handlerb req

-- TODO: figure out how to deduplicate the two instances below:
instance (HasServer s x) => HasServer (GET :> s) x where
  route :: Proxy (a :> s) -> Server s -> (T.Text, [T.Text]) -> Maybe (AppCtx x)
  route _ handler (method, xs) = if "GET" == method then route (Proxy :: Proxy s) handler (method, xs) else Nothing

instance (HasServer s x) => HasServer (POST :> s) x where
  route :: Proxy (a :> s) -> Server s -> (T.Text, [T.Text]) -> Maybe (AppCtx x)
  route _ handler (method, xs) = if "POST" == method then route (Proxy :: Proxy s) handler (method, xs) else Nothing

instance (KnownSymbol s, HasServer r x) => HasServer ((s :: Symbol) :> r) x where
  route :: Proxy (s :> r) -> Server r -> (T.Text, [T.Text]) -> Maybe (AppCtx x)
  route _ handler (method, x : xs) = if T.pack (symbolVal (Proxy :: Proxy s)) == x then route (Proxy :: Proxy r) handler (method, xs) else Nothing
  route _ _ _ = Nothing

instance (Show a, Read a, HasServer r x) => HasServer (Capture a :> r) x where
  route :: Proxy (Capture a :> r) -> (a -> Server r) -> (T.Text, [T.Text]) -> Maybe (AppCtx x)
  route _ handler (method, x : xs) = readMaybe (T.unpack x) >>= \a -> route (Proxy :: Proxy r) (handler a) (method, xs)
  route _ _ _ = Nothing

serve :: (HasServer layout a) => Proxy layout -> Server layout -> Request -> AppCtx a
serve proxy server request = fromMaybe (throwError (Error404 "not found")) (route proxy server (T.decodeUtf8 $ requestMethod request, pathInfo request))
