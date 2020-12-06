{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AppContext where

import Control.Monad ((>=>)) -- SUMMON THE KALA
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..))
import Data.Bifunctor
import qualified Data.ByteString.Lazy as LBS

data Config = Config
  { db :: LBS.ByteString,
    hardcodedResponse :: LBS.ByteString
  }

newtype Env = Env
  { config :: Config
  }

data Error = Error404 LBS.ByteString | Error500 LBS.ByteString

-- AppContext is a monad that has read access to an Env, performs some IO, and returns either e or t
newtype AppContext e t = AppContext {runWithCtx :: Env -> IO (Either e t)}

-- Generally type e will always be Error in our application, while t is some generic success return type
-- We defined AppContext as * -> * -> * above simply because we need it to define e.g. Bifunctor instance for AppContext
type AppCtx = AppContext Error

runApp :: AppCtx a -> Env -> IO (Either Error a)
runApp = runWithCtx

instance Functor (AppContext e) where
  fmap func app = AppContext $ runWithCtx app >=> pure . fmap func

instance Applicative (AppContext e) where
  pure :: a -> AppContext e a
  pure = AppContext . const . return . Right

  (<*>) :: AppContext e (a -> b) -> AppContext e a -> AppContext e b
  (<*>) appWithFn appWithValue = AppContext $
    \env -> do
      eithFn <- runWithCtx appWithFn env
      eith <- runWithCtx appWithValue env
      return $ case (eith, eithFn) of
        (_, Left x) -> Left x
        (Left x, _) -> Left x
        (Right val, Right fn) -> Right $ fn val

instance Monad (AppContext e) where
  (>>=) :: AppContext e a -> (a -> AppContext e b) -> AppContext e b
  (>>=) appA fnThatReturnsAppB = AppContext $
    \env -> do
      eith <- runWithCtx appA env
      case eith of
        Left e -> return $ Left e
        Right a -> runWithCtx (fnThatReturnsAppB a) env

instance Bifunctor AppContext where
  bimap :: (e -> e2) -> (t -> t2) -> AppContext e t -> AppContext e2 t2
  bimap f1 f2 app = AppContext $
    \env -> do
      eith <- runWithCtx app env
      return $ bimap f1 f2 eith

-- needs MultiParamTypeClasses
instance MonadReader Env (AppContext e) where
  -- Return the Env
  ask :: AppContext e Env
  ask = AppContext $ \env -> pure . Right $ env

  -- Run using modified Env value (fn env)
  local :: (Env -> Env) -> AppContext e a -> AppContext e a
  local fn app = AppContext $ \env -> runWithCtx app $ fn env

  -- Return an Env modified by fn
  reader :: (Env -> a) -> AppContext e a
  reader fn = AppContext $ \env -> pure . Right $ fn env

instance MonadIO (AppContext e) where
  -- Lift an IO a to AppContext e a
  liftIO :: IO a -> AppContext e a
  liftIO = AppContext . const . fmap Right

liftEither :: Either e a -> AppContext e a
liftEither = AppContext . const . pure

getConfig :: AppCtx Config
getConfig = do config <$> ask
