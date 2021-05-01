module FancyRouting where

import AppContext
import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Except
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Lazy.Char8 as LB
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.TypeLits
import JSON
import Network.Wai
  ( Request,
    lazyRequestBody,
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

data BodyParser (a :: *)

type family Server layout :: *

type instance Server (Respond a) = AppCtx a

type instance Server (a :<|> b) = Server a :<|> Server b

type instance Server ((s :: Symbol) :> r) = Server r

type instance Server (GET :> r) = Server r

type instance Server (POST :> r) = Server r

type instance Server (Capture a :> r) = a -> Server r

type instance Server (BodyParser a :> r) = a -> Server r

class HasServer layout a z where
  route :: Proxy layout -> Server layout -> (T.Text, [T.Text], z) -> Maybe (AppCtx a)

instance (ToJSON a) => HasServer (Respond a) JSON z where
  route _ handler (_, [], _) = Just (toJSON <$> handler)
  route _ _ _ = Nothing

instance (HasServer a x z, HasServer b x z) => HasServer (a :<|> b) x z where
  route :: Proxy (a :<|> b) -> (Server a :<|> Server b) -> (T.Text, [T.Text], z) -> Maybe (AppCtx x)
  route _ (handlera :<|> handlerb) req = route (Proxy :: Proxy a) handlera req <|> route (Proxy :: Proxy b) handlerb req

-- TODO: figure out how to deduplicate the two instances below:
instance (HasServer s x z) => HasServer (GET :> s) x z where
  route :: Proxy (a :> s) -> Server s -> (T.Text, [T.Text], z) -> Maybe (AppCtx x)
  route _ handler (method, xs, z) = if "GET" == method then route (Proxy :: Proxy s) handler (method, xs, z) else Nothing

instance (HasServer s x z) => HasServer (POST :> s) x z where
  route :: Proxy (a :> s) -> Server s -> (T.Text, [T.Text], z) -> Maybe (AppCtx x)
  route _ handler (method, xs, z) = if "POST" == method then route (Proxy :: Proxy s) handler (method, xs, z) else Nothing

instance (KnownSymbol s, HasServer r x z) => HasServer ((s :: Symbol) :> r) x z where
  route :: Proxy (s :> r) -> Server r -> (T.Text, [T.Text], z) -> Maybe (AppCtx x)
  route _ handler (method, x : xs, z) = if T.pack (symbolVal (Proxy :: Proxy s)) == x then route (Proxy :: Proxy r) handler (method, xs, z) else Nothing
  route _ _ _ = Nothing

instance (Show a, Read a, HasServer r x z) => HasServer (Capture a :> r) x z where
  route :: Proxy (Capture a :> r) -> (a -> Server r) -> (T.Text, [T.Text], z) -> Maybe (AppCtx x)
  route _ handler (method, x : xs, z) = readMaybe (T.unpack x) >>= \a -> route (Proxy :: Proxy r) (handler a) (method, xs, z)
  route _ _ _ = Nothing

instance (FromJSON a, HasServer r x ByteString, HasServer r x a) => HasServer (BodyParser a :> r) x ByteString where
  route :: Proxy (BodyParser a :> r) -> (a -> Server r) -> (T.Text, [T.Text], ByteString) -> Maybe (AppCtx x)
  route _ handler (method, xs, z) =
    case parseBody z of
      Left e -> Just (throwError e)
      Right val -> route (Proxy :: Proxy r) (handler val) (method, xs, val)

parseBody :: (FromJSON a) => ByteString -> Either Error a
parseBody body = first (const $ Error400 "Invalid request body") $ tokenize (LB.unpack body) >>= jsonParse >>= fromJSON

serve :: (HasServer layout a ByteString) => Proxy layout -> Server layout -> Request -> AppCtx a
serve proxy server request = AppContext $ \env -> do
  body <- lazyRequestBody request
  let appl = fromMaybe (throwError (Error404 "not found")) (route proxy server (T.decodeUtf8 $ requestMethod request, pathInfo request, body))
  runWithCtx appl env
