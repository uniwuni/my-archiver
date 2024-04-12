-- |
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module SoundcloudArchiver where

import GHC.Generics
import Data.Text
import Data.Aeson
import System.FilePath
--import Data.RDF hiding (Query, triple)
import Data.Text
import Control.Monad.Trans.State
import qualified Data.Text as T
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator
import qualified Data.RDF as RDF
import Control.Monad (join, when, sequence)
import Data.Either
import Data.Maybe
import System.Directory
import Config
import Data.Scientific


import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Abbrevs
import Utils
import Term
import Byline.Menu
import qualified YoutubeArchiver as Y

data TrackJSONInfo = TrackJSONInfo {
  id :: Text,
  title :: Text,
  thumbnail :: Text,
  description :: Text,
  uploader_url :: Text, --channel_url
  uploader_id :: Text, --channel_id
  duration :: Scientific,
  uploader :: Text, --channel
  timestamp :: Integer, --uploaddate
  webpage_url :: Text,
  epoch :: Integer,
  genre :: Text
} deriving (Generic, Show, Eq)

id_ (TrackJSONInfo x _ _ _ _ _ _ _ _ _ _ _) = x
instance FromJSON TrackJSONInfo where
soundcloudAccountAdd :: TrackJSONInfo -> IRIRef -> Query UpdateQuery
soundcloudAccountAdd info uploaderIri = do
  list_ <- prefixes
  let [owl,rdf',rdfs,xsd,foaf,frbr,dce,dct,unic,unia,fabio,mo] = list_
  let channelUploader = iriRef $ uploader_url info
  updateTriple uploaderIri (foaf .:. "name") (uploader info)
  updateTriple uploaderIri (foaf .:. "account") channelUploader
  updateTriple channelUploader rType (foaf .:. "Account")
  updateTriple channelUploader (foaf .:. "accountServiceHomepage") (iriRef "https://soundcloud.com")
  updateTriple channelUploader (foaf .:. "accountName") (uploader info)
  update

uploaderAdd :: TrackJSONInfo -> (TrackJSONInfo -> IRIRef -> Query UpdateQuery) -> Query UpdateQuery
uploaderAdd info moreInfo = do
  list_ <- prefixes
  let [owl,rdf',rdfs,xsd,foaf,frbr,dce,dct,unic,unia,fabio,mo] = list_
  -- stupidest workaround of all time
  let upload = iriRef $ "https://uniwuni.github.io/archives#" <> encodeText (uploader info)  <> "-" <> T.drop 23 (uploader_url info) <> "-sc"
  updateTriple upload rType (foaf .:. "Agent")
  moreInfo info upload


uploaderAdd' :: Config -> TrackJSONInfo -> IO RDF.Node
uploaderAdd' cfg info = do
  r <- Y.channelExistsSelect' cfg (uploader_url info)
  case r of
    Nothing -> runBylineT (uploaderAdd'' (uploader info)) >>= (maybe (error "Couldn't choose uploader!") (pure . RDF.UNode))
    Just iri -> pure iri
  where uploaderAdd'' name = do
          sayLn $ text $ "Query: " <> name
          sayLn $ text $ "Song: " <> title info
          res <- chooseFromSelect cfg (agentSelect name) "Choose an agent:"
          case res of
            OtherQuery -> do searchTerm <- askLn (text "Agent search term: ") (Just name)
                             uploaderAdd'' searchTerm
            MakeNew ->
              do res <- lift $ updateQuery (updateEndpoint cfg) (uploaderAdd info soundcloudAccountAdd)
                 if res then pure ("https://uniwuni.github.io/archives#" <> encodeText (uploader info)  <> "-" <> T.drop 23 (uploader_url info) <> "-sc") else error ("Adding uploader for " ++ show info ++ " failed!")
            Result (Match (Bound (RDF.UNode a):_ )) -> do
              b <- lift $ updateQuery (updateEndpoint cfg) $ soundcloudAccountAdd info $ iriRef a
              if b then pure a else lift $ error "Something bad happened!"
            Result (Match _) -> lift (putStrLn "Incorrect type!") >> uploaderAdd'' name
            Result (Other text) -> lift (putStrLn "Very unlikely that this is what you want!") >> uploaderAdd'' name
            


trackAdd :: (SubjectTermLike a, ObjectTermLike a) => FilePath -> Maybe FilePath -> Config -> TrackJSONInfo -> a -> Query UpdateQuery
trackAdd basename separateImage cfg info uploader = do
      list_ <- prefixes
      let [owl,rdf',rdfs,xsd,foaf,frbr,dce,dct,unic,unia,fabio, mo] = list_
      let musicWork = unia .:. (encodeText (title info) <> "-" <> id_ info <> "-sc-work")
      let signal = unia .:. (encodeText (title info) <> "-" <> id_ info <> "-sc-expr")
      let audioStream = iriRef $ webpage_url info
      let audioFile = unia .:. (encodeText (title info) <> "-" <> id_ info <> "-sc-man")
      let actualAudioFile = iriRef (archivePrefix cfg <> encodePath (T.pack $ soundcloudFolder cfg </> basename))

      let genreConcept = iriRef $ "https://uniwuni.github.io/archives#" <> (encodeText (genre info) <> "-genre")
      let thumbnailWork = unia .:. (encodeText (title info) <> "-" <> id_ info <> "-sc-thumbnail-work")
      let thumbnailImage = unia .:. (encodeText (title info) <> "-" <> id_ info <> "-sc-thumbnail-expr")
      let thumbnailImageFile = unia .:. (encodeText (title info) <> "-" <> id_ info <> "-sc-thumbnail-man")
      let thumbnailRemote = iriRef $ thumbnail info
      let actualThumbnailFile = maybe actualAudioFile (iriRef . (archivePrefix cfg <>) . encodePath . T.pack) separateImage
      let dur2 = (round $ duration info * 1000) :: Integer

      when (not $ T.null $ genre info) $ (do
        updateTriple genreConcept rType (iriRef "http://www.w3.org/2004/02/skos/core#Concept")
        updateTriple genreConcept rType (mo .:. "Genre")
        updateTriple audioStream (mo .:. "genre") genreConcept
        updateTriple signal (mo .:. "genre") genreConcept
        updateTriple_ audioFile (mo .:. "genre") genreConcept)

      updateTriple musicWork rType (frbr .:. "Work")
      updateTriple musicWork rType (fabio .:. "SoundRecording")
      updateTriple musicWork rType (mo .:. "MusicalWork")
      updateTriple musicWork (frbr .:. "realization") signal
      updateTriple musicWork (dce .:. "title") (title info)

      updateTriple signal rType (frbr .:. "Expression")
      updateTriple signal rType (fabio .:. "AudioDocument")
      updateTriple signal rType (mo .:. "Signal")
      updateTriple signal (frbr .:. "embodiment") audioFile
      updateTriple signal (frbr .:. "embodiment") audioStream
      updateTriple signal (dce .:. "title") (title info)
      updateTriple signal (mo .:. "publishedAs") audioStream

      updateTriple audioStream rType (frbr .:. "Manifestation")
      updateTriple audioStream rType (fabio .:. "WebManifestation")
      updateTriple audioStream rType (mo .:. "MusicalManifestation")
      updateTriple audioStream (frbr .:. "producer") uploader
      updateTriple audioStream (mo .:. "producer") uploader
      updateTriple audioStream (frbr .:. "reproduction") audioFile
      updateTriple audioStream (dce .:. "date") (epochToXSDDateTime $ timestamp info, xsd .:. "dateTime")
      updateTriple audioStream (dce .:. "title") (title info)
      updateTriple audioStream (dce .:. "description") (description info)
      updateTriple audioStream (dct .:. "extent") (dur2 `div` 1000)
      updateTriple audioStream (mo .:. "duration") dur2

      updateTriple audioFile rType (frbr .:. "Manifestation")
      updateTriple audioFile rType (fabio .:. "DigitalManifestation")
      updateTriple audioFile rType (mo .:. "MusicalManifestation")
      updateTriple audioFile (frbr .:. "exemplar") actualAudioFile
      updateTriple audioFile (mo .:. "item") actualAudioFile
      updateTriple audioFile (dce .:. "title") (title info)
      updateTriple audioFile (dce .:. "description") (description info)
      updateTriple audioFile (dce .:. "date") (epochToXSDDateTime $ epoch info, xsd .:. "dateTime")
      updateTriple audioFile (dct .:. "extent") (dur2 `div` 1000)
      updateTriple audioFile (mo .:. "duration") dur2


      updateTriple actualAudioFile rType (frbr .:. "Item")
      updateTriple actualAudioFile rType (mo .:. "AudioFile")
      updateTriple actualAudioFile rType (fabio .:. "ComputerFile")
      updateTriple actualAudioFile (frbr .:. "owner") (unic .:. "me")
      updateTriple actualAudioFile (mo .:. "encodes") signal

      updateTriple thumbnailWork rType (frbr .:. "Work")
      updateTriple thumbnailWork (frbr .:. "realization") thumbnailImage
      updateTriple thumbnailWork rType (fabio .:. "StillImage")

      updateTriple thumbnailImage rType (frbr .:. "Expression")
      updateTriple thumbnailImage rType (fabio .:. "Cover")
      updateTriple thumbnailImage (frbr .:. "complement") signal
      updateTriple thumbnailImage (frbr .:. "embodiment") thumbnailImageFile
      updateTriple thumbnailImage (frbr .:. "embodiment") thumbnailRemote

      updateTriple thumbnailImageFile rType (frbr .:. "Manifestation")
      updateTriple thumbnailImageFile rType (fabio .:. "DigitalManifestation")
      updateTriple thumbnailImageFile (dce .:. "date") (epochToXSDDateTime $ epoch info, xsd .:. "dateTime")
      updateTriple thumbnailImageFile (frbr .:. "exemplar") actualThumbnailFile

      updateTriple thumbnailRemote rType (frbr .:. "Manifestation")
      updateTriple thumbnailRemote (frbr .:. "producer") uploader
      updateTriple thumbnailRemote (frbr .:. "reproduction") audioFile
      updateTriple thumbnailRemote (dce .:. "date") (epochToXSDDateTime $ timestamp info, xsd .:. "dateTime")
      updateTriple thumbnailRemote rType (fabio .:. "WebManifestation")

      updateTriple actualThumbnailFile rType (frbr .:. "Item")
      updateTriple actualThumbnailFile rType (fabio .:. "ComputerFile")
      updateTriple actualThumbnailFile (frbr .:. "owner") (unic .:. "me")

      update

runQueries :: Config -> Bool -> FilePath -> IO Bool
runQueries cfg redo basename = do
  let json_file = mountPoint cfg </> soundcloudFolder cfg </> basename -<.> "info.json"
  jsonReal <- doesFileExist json_file
  if jsonReal then do
    let jpeg = soundcloudFolder cfg </> basename -<.> "jpg"
    jpegEx <- doesFileExist $ mountPoint cfg </> jpeg
    let image = if jpegEx then Just jpeg else Nothing
    decoded <- decodeFileStrict json_file :: IO (Maybe TrackJSONInfo)
    case decoded of
      Nothing -> (putStrLn ("JSON " ++ json_file ++ " malformed!")) >> return False
      Just metadata -> do
        upload <- uploaderAdd' cfg metadata
        alreadyThere <- askQuery (selectEndpoint cfg) $ Y.isEmbodiedAsk $ webpage_url metadata
        if not redo && alreadyThere then putStrLn (basename ++ ": Already exists!") else do
          --putStrLn "doing video"
          res <- updateQuery (updateEndpoint cfg) $ trackAdd basename image cfg metadata upload
          if res then pure () else error ("Adding video info for " ++ show metadata ++ " failed!")
          putStrLn $ "Successfully added: " ++ basename
          when jpegEx $ putStrLn "Added outside thumbnail!"
        return True
  else putStrLn "json does not exist!" >> return False
