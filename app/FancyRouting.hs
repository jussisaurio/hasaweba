module FancyRouting where

import AppContext
import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Except
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import GHC.TypeLits
import JSON
import Text.Read

-- Adapted (stolen) from https://www.well-typed.com/blog/2015/11/implementing-a-minimal-version-of-haskell-servant/
data Respond (a :: *)

data a :<|> b = a :<|> b

infixr 8 :<|>

data (a :: k) :> (b :: *)

infixr 9 :>

data Capture (a :: *)

type family Server layout :: *

type instance Server (Respond a) = AppCtx a

type instance Server (a :<|> b) = Server a :<|> Server b

type instance Server ((s :: Symbol) :> r) = Server r

type instance Server (Capture a :> r) = a -> Server r

class HasServer layout a where
  route :: Proxy layout -> Server layout -> [T.Text] -> Maybe (AppCtx a)

instance (ToJSON a) => HasServer (Respond a) JSON where
  route _ handler [] = Just (toJSON <$> handler)
  route _ _ _ = Nothing

instance (HasServer a x, HasServer b x) => HasServer (a :<|> b) x where
  route :: Proxy (a :<|> b) -> (Server a :<|> Server b) -> [T.Text] -> Maybe (AppCtx x)
  route _ (handlera :<|> handlerb) xs = route (Proxy :: Proxy a) handlera xs <|> route (Proxy :: Proxy b) handlerb xs

instance (KnownSymbol s, HasServer r x) => HasServer ((s :: Symbol) :> r) x where
  route :: Proxy (s :> r) -> Server r -> [T.Text] -> Maybe (AppCtx x)
  route _ handler (x : xs) = if T.pack (symbolVal (Proxy :: Proxy s)) == x then route (Proxy :: Proxy r) handler xs else Nothing
  route _ _ _ = Nothing

instance (Show a, Read a, HasServer r x) => HasServer (Capture a :> r) x where
  route :: Proxy (Capture a :> r) -> (a -> Server r) -> [T.Text] -> Maybe (AppCtx x)
  route _ handler (x : xs) = readMaybe (T.unpack x) >>= \a -> route (Proxy :: Proxy r) (handler a) xs
  route _ _ _ = Nothing

serve :: (HasServer layout a) => Proxy layout -> Server layout -> [T.Text] -> AppCtx a
serve proxy server path = fromMaybe (throwError (Error404 "not found")) (route proxy server path)
