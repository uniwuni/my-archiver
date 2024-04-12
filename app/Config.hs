-- |
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Config where


import GHC.Generics
import Data.Text
import Data.Aeson
import System.FilePath
--import Data.RDF hiding (Query, triple)
import Data.Text
import qualified Data.Text as T
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator
import qualified Data.RDF as RDF
import Control.Monad (join, when)
import Data.Either
import Data.Maybe
import System.Directory

data Config = Config {
  youtubeFolder :: FilePath,
  soundcloudFolder :: FilePath,
  selectEndpoint :: EndPoint,
  updateEndpoint :: EndPoint,
  mountPoint :: FilePath,
  archivePrefix :: Text,
  musicUploaders :: [Text],
  nonMusicUploaders :: [Text]
} deriving (Generic, Show)

instance FromJSON Config where

myConfig = Config "Archive/Videos/Youtube History/ytvideos" "Archive/Musik/Soundcloud Likes/" "http://localhost:3030/archive/"
  "http://localhost:3030/archive/" "/run/media/uni/Neoproterozoikum"
  "http://localhost:9999/" [] []
