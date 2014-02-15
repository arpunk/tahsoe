{-# LANGUAGE RankNTypes #-}
module Network.TAHOE (
   Filesystem(..),
   Directory(..),
   File(..),
   Statistics(..),
   TahoeT
  ) where

import qualified Data.Text as T

import Control.Applicative ((<$>))

import Data.ByteString.Lazy (fromStrict)
import Control.Exception.Base (Exception)
import Control.Exception.Lifted (throwIO, catch)

import Data.Conduit (runResourceT)
import Network.HTTP.Conduit (parseUrl, newManager, httpLbs, RequestBody(..), Response(..), HttpException(..), Request(..), Manager)

import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (ResponseHeaders)

import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (logDebug, LoggingT, runStderrLoggingT, MonadLogger, NoLoggingT(..))
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Default (def)
import Data.Aeson (FromJSON, encode, decode)

import Network.TAHOE.Monad
import Network.TAHOE.Types
import Network.TAHOE.HTTP

newTahoeEnv :: MonadIO m => T.Text -> Int -> m TahoeEnv
newTahoeEnv host port = do
  mgr <- liftIO $ newManager def
  return TahoeEnv {
      tlRootHost = host
    , tlRootPort = port
    , tlRootMgr  = mgr
    }

  where
    uri = T.concat "http://" ++ host ++ ":" ++ (T.pack $ show port) ++ "/"

runTahoeT :: MonadIO m => TahoeEnv -> TahoeT m a -> m a
runTahoeT config action =
  runReaderT action config

runTahoe :: MonadIO m => (TahoeEnv -> TahoeT (NoLoggingT IO) a -> m a)
runTahoe config action =
  liftIO $ runNoLoggingT $ runReaderT action config

runTahoeLogging :: MonadIO m => (TahoeEnv -> TahoeT (LoggingT IO) a -> m a)
runTahoeLogging config action =
  liftIO $ runStderrLoggingT $ runReaderT action config

query' :: (FromJSON x) => T.Text -> T.Text -> T.Text -> TahoeT m x
query' section method request = do
  conf <- ask
  initReq <- liftIO $ parseUrl $ T.unpack $ tlRootHost conf

  let req = initReq { requestBody = RequestBodyLBS $ encode request
                    , method = methodPost }

  response <- catch (liftIO $ runResourceT (httpLbs req $ tlRootMgr conf)) catchHttpException

  case decode $ responseBody response of
    Just result -> return result
    Nothing -> throwIO "error"

  where
    catchHttpException :: HttpException -> TahoeT m a
    catchHttpException e@(StatusCodeException _ headrs _) = do
      maybe (throwIO e) throwIO (decodeError headrs)
    catchHttpException e = throwIO e

    decodeError :: ResponseHeaders -> Maybe TahoeError
    decodeError headers = fromStrict <$> lookup "X-Response-Body-Start" headers >>= decode
