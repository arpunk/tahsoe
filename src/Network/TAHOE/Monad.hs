{-# LANGUAGE RankNTypes #-}
module Network.TAHOE.Monad where

import qualified Data.Text as T

import           Control.Monad.Reader (ReaderT, MonadReader)
import           Control.Monad.Logger (logDebug, LoggingT, runStderrLoggingT, MonadLogger, NoLoggingT(..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.IO.Class (MonadIO)
import           Network.TAHOE.Types
import           Network.HTTP.Conduit (Manager)

data TahoeEnv = TahoeEnv {
    tlRootHost :: T.Text
  , tlRootPort :: Int
  , tlRootMgr  :: Manager  
  }

type TahoeT m a = (MonadIO m, MonadLogger m, MonadBaseControl IO m) => ReaderT TahoeEnv m a
