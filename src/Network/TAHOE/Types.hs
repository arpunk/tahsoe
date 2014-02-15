{-# LANGUAGE OverloadedStrings #-}
module Network.TAHOE.Types (
  Filesystem(..),
  File(..),
  Directory(..),
  Statistics(..),
  TahoeAction,
  TahoeError
  ) where

import Data.Maybe
import Data.Either
import Data.Aeson

import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>), pure)

import Data.Text (Text)

type Hostname = Text
type Port = Int
type URI = Text

data TahoeAction = UploadFile
                 | RenameFile
                 | DownloadFile
                 | AttachFile
                 | DeleteFile
                 | CreateDir
                 | RemoveDir
                 | RenameDir

data TahoeError = Algo Int Text Text
                | Otro Int Text Text  

-- | This is the root CAP URI
data Filesystem = Filesystem {
    fsUri :: URI
  }

data File = File {
    fileDir         :: Directory
  , fileUri         :: URI
  , fileName        :: Text  
  , fileIsMutable   :: Bool
  , fileIsWriteable :: Bool  
  }

data Directory = Directory {
    dirFs          :: Filesystem
  , dirUri         :: URI
  , dirName        :: Text  
  , dirIsMutable   :: Bool  
  , dirIsWriteable :: Bool
  , hasChildren    :: Maybe [File]  -- We must prefetch them
  } 

data Statistics = Statistics {
    statsLoadMonitor :: Text
  , statsCpuMonitor  :: Text
  , statsUploader    :: Text
  , statsDownloader  :: Text
  , statsPublishes   :: Int
  , statsRetreives   :: Int
  }

instance FromJSON Statistics where
  parseJSON (Object v) = Statistics <$> v .: "statsLoadMonitor"
                                    <*> v .: "statsCpuMonitor"
                                    <*> v .: "statsUploader"
                                    <*> v .: "statsDownloader"
                                    <*> v .: "statsPublishes"
                                    <*> v .: "statsRetreives"
  parseJSON _ = mzero
