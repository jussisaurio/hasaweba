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
data Respond (a :: *) (t :: *)

data GET

data POST

class ResponseType a where
  encode :: a -> ByteString

instance ResponseType JSON where
  encode = LB.pack . jsonSerialize

instance ResponseType String where
  encode = LB.pack

data a :<|> b = a :<|> b

infixr 8 :<|>

data (a :: k) :> (b :: *)

infixr 9 :>

data Capture (a :: *)

data BodyParser (a :: *)

type family Server layout :: *

type instance Server (Respond a t) = AppCtx a

type instance Server (a :<|> b) = Server a :<|> Server b

type instance Server ((s :: Symbol) :> r) = Server r

type instance Server (GET :> r) = Server r

type instance Server (POST :> r) = Server r

type instance Server (Capture a :> r) = a -> Server r

type instance Server (BodyParser a :> r) = a -> Server r

type Method = T.Text

type PathParts = [T.Text]

class HasServer layout returnT body where
  route :: Proxy layout -> Server layout -> (Method, PathParts, body) -> Maybe (AppCtx returnT)

instance (ToJSON a) => HasServer (Respond a JSON) ByteString body where
  route _ handler (_, [], _) = Just (encode . toJSON <$> handler)
  route _ _ _ = Nothing

instance (Show a) => HasServer (Respond a String) ByteString body where
  route _ handler (_, [], _) = Just (encode . show <$> handler)
  route _ _ _ = Nothing

instance (HasServer a returnT body, HasServer b returnT body) => HasServer (a :<|> b) returnT body where
  route :: Proxy (a :<|> b) -> (Server a :<|> Server b) -> (Method, PathParts, body) -> Maybe (AppCtx returnT)
  route _ (handlera :<|> handlerb) req = route (Proxy :: Proxy a) handlera req <|> route (Proxy :: Proxy b) handlerb req

-- TODO: figure out how to deduplicate the two instances below:
instance (HasServer s returnT body) => HasServer (GET :> s) returnT body where
  route :: Proxy (a :> s) -> Server s -> (Method, PathParts, body) -> Maybe (AppCtx returnT)
  route _ handler (method, xs, z) = if "GET" == method then route (Proxy :: Proxy s) handler (method, xs, z) else Nothing

instance (HasServer s returnT body) => HasServer (POST :> s) returnT body where
  route :: Proxy (a :> s) -> Server s -> (Method, PathParts, body) -> Maybe (AppCtx returnT)
  route _ handler (method, xs, z) = if "POST" == method then route (Proxy :: Proxy s) handler (method, xs, z) else Nothing

instance (KnownSymbol s, HasServer r returnT body) => HasServer ((s :: Symbol) :> r) returnT body where
  route :: Proxy (s :> r) -> Server r -> (Method, PathParts, body) -> Maybe (AppCtx returnT)
  route _ handler (method, x : xs, z) = if T.pack (symbolVal (Proxy :: Proxy s)) == x then route (Proxy :: Proxy r) handler (method, xs, z) else Nothing
  route _ _ _ = Nothing

instance (Read a, HasServer r returnT body) => HasServer (Capture a :> r) returnT body where
  route :: Proxy (Capture a :> r) -> (a -> Server r) -> (Method, PathParts, body) -> Maybe (AppCtx returnT)
  route _ handler (method, x : xs, z) = readMaybe (T.unpack x) >>= \a -> route (Proxy :: Proxy r) (handler a) (method, xs, z)
  route _ _ _ = Nothing

instance (FromJSON parsedBodyT, HasServer r returnT ByteString, HasServer r returnT parsedBodyT) => HasServer (BodyParser parsedBodyT :> r) returnT ByteString where
  route :: Proxy (BodyParser parsedBodyT :> r) -> (parsedBodyT -> Server r) -> (Method, PathParts, ByteString) -> Maybe (AppCtx returnT)
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
