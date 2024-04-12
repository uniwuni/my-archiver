{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module YoutubeArchiver where

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
import Control.Monad (join, when, sequence, forM_)
import Data.Either
import Data.Maybe
import System.Directory
import Config
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Abbrevs
import Utils
import Byline.Menu
import Control.Applicative
data VideoJSONInfo = VideoJSONInfo {
  id :: Text,
  title :: Text,
  thumbnail :: Text,
  description :: Text,
  channel_url :: Text,
  channel_id :: Text,
  duration :: Integer,
  channel :: Text,
  upload_date :: Text,
  language :: Maybe Text,
  webpage_url :: Text,
  epoch :: Integer
} deriving (Generic, Show, Eq)

dateFormatToDate :: Text -> Text
dateFormatToDate t = year <> "-" <> month <> "-" <> day <> "T00:00:00Z"
  where (year, rest) = T.splitAt 4 t
        (month, day) = T.splitAt 2 rest

id_ (VideoJSONInfo i _ _ _ _ _ _ _ _ _ _ _) = i

instance FromJSON VideoJSONInfo where

channelExistsSelect :: Text -> Query SelectQuery
channelExistsSelect url = do
  list_ <- prefixes
  let [owl,rdf',rdfs,xsd,foaf,frbr,dce,dct,unic,unia,fabio,mo] = list_
  agent <- var
  triple_ agent rType (foaf .:. "Agent")
  triple_ agent (foaf .:. "account") (iriRef url)
  selectVars [agent]

isEmbodiedAsk :: Text -> Query AskQuery
isEmbodiedAsk url = do
  list_ <- prefixes
  let [owl,rdf',rdfs,xsd,foaf,frbr,dce,dct,unic,unia,fabio,mo] = list_
  video <- var
  askTriple video rType (frbr .:. "Expression")
  askTriple video (frbr .:. "embodiment") (iriRef url)
  ask


channelExistsSelect' :: Config -> Text -> IO (Maybe RDF.Node)
channelExistsSelect' config url = do
  results <- selectQuery (selectEndpoint config) (channelExistsSelect url)
  return $ ifTuple results
  where ifTuple (Just ([Bound a]:_)) = Just a
        ifTuple _ = Nothing


youtubeAccountAdd :: VideoJSONInfo -> IRIRef -> Query UpdateQuery
youtubeAccountAdd info uploaderIri = do
  list_ <- prefixes
  let [owl,rdf',rdfs,xsd,foaf,frbr,dce,dct,unic,unia,fabio,mo] = list_
  let channelUploader = iriRef $ channel_url info
  updateTriple uploaderIri (foaf .:. "name") (channel info)
  updateTriple uploaderIri (foaf .:. "account") channelUploader
  updateTriple channelUploader rType (foaf .:. "Account")
  updateTriple channelUploader (foaf .:. "accountServiceHomepage") (iriRef "https://youtube.com")
  updateTriple channelUploader (foaf .:. "accountName") (channel info)
  update

uploaderAdd :: VideoJSONInfo -> (VideoJSONInfo -> IRIRef -> Query UpdateQuery) -> Query UpdateQuery
uploaderAdd info moreInfo = do
  list_ <- prefixes
  let [owl,rdf',rdfs,xsd,foaf,frbr,dce,dct,unic,unia,fabio,mo] = list_
  -- stupidest workaround of all time
  let uploader = iriRef $ "https://uniwuni.github.io/archives#" <> encodeText (channel info <> "-" <> channel_id info <>"-yt")
  updateTriple uploader rType (foaf .:. "Agent")
  moreInfo info uploader

uploaderAdd' :: Config -> VideoJSONInfo -> IO ()
uploaderAdd' cfg info = do
  res <- updateQuery (updateEndpoint cfg) (uploaderAdd info youtubeAccountAdd)
  if res then pure () else error ("Adding uploader for " ++ show info ++ " failed!")

isMusicCheckedAsk :: Text -> Query AskQuery
isMusicCheckedAsk url = do
  list_ <- prefixes
  let [owl,rdf',rdfs,xsd,foaf,frbr,dce,dct,unic,unia,fabio,mo] = list_
  bool <- var
  askTriple (iriRef url) (unia .:. "representsMusic") bool
  ask

exprWorkSelect :: Query SelectQuery
exprWorkSelect = do
    list_ <- prefixes
    let [owl,rdf',rdfs,xsd,foaf,frbr,dce,dct,unic,unia,fabio,mo] = list_
    expr <- var
    work <- var
    man <- var
    upl <- var
    title <- var
    name <- var
    duration <- var
    acc <- var
    triple_ work (frbr .:. "realization") expr
    triple_ work rType (fabio .:. "MovingImage")
    triple_ expr rType (fabio .:. "Movie")
    triple_ man rType (fabio .:. "WebManifestation")
    triple_ expr (frbr .:. "embodiment") man
    triple_ man (frbr .:. "producer") upl
    triple_ upl (foaf .:. "name") name
    triple_ upl (foaf .:. "account") acc
    triple_ work (dce .:. "title") title
    triple_ man (dct .:. "extent") duration
    v <- var
    filterNotExists $ triple_ expr (unia .:. "representsMusic") v
    d0 <- var
    d1 <- var
    d2 <- var
    d3 <- var
    d4 <- var
    groupBy_ expr
    groupBy_ work
    select [SelectVar expr,
            SelectVar work,
            sample man `as` d0,
            sample acc `as` d4,
            groupConcat title "; " `as` d1,
            groupConcat name "; " `as` d2,
            avg duration `as` d3]


exprWorkSelect' :: Config -> IO [(Text, Text, Text, Text, Text, Text, Text)]
exprWorkSelect' config = do
  results <- selectQuery (selectEndpoint config) exprWorkSelect
  return $ ifTuple results
  where ifTuple (Just xs) = (\[Bound (RDF.UNode x), Bound (RDF.UNode y), Bound (RDF.UNode v), Bound (RDF.UNode a),
                               Bound (RDF.LNode z), Bound (RDF.LNode w), Bound (RDF.LNode (RDF.TypedL l _))] -> (x,y,v,a,showLValue z,showLValue w,l)) <$> xs
        ifTuple _ = []


-- wenn es mehrere realized dann lieber nicht machen sonst halt nach dem einen suchen und dann musicalWorkAdd
-- damit und terminal mit yes/no/skip fragen ob music

musicalWorkAdd :: Text -> Text -> Bool -> Query UpdateQuery
musicalWorkAdd expression work True = do
      list_ <- prefixes
      let [owl,rdf',rdfs,xsd,foaf,frbr,dce,dct,unic,unia,fabio,mo] = list_
      let musicWork = iriRef $ expression <> "-audio-work"
      let musicExpr = iriRef $ expression <> "-audio-expr"

      updateTriple musicWork rType (frbr .:. "Work")
      updateTriple musicWork rType (fabio .:. "SoundRecording")
      updateTriple musicWork rType (mo .:. "MusicalWork")
      updateTriple musicWork (frbr .:. "realization") musicExpr

      updateTriple musicExpr rType (frbr .:. "Expression")
      updateTriple musicExpr rType (fabio .:. "AudioDocument")
      updateTriple musicExpr rType (mo .:. "Signal")

      updateTriple (iriRef expression) (unia .:. "representsMusic") True

      updateTriple (iriRef work) (frbr .:. "part") musicWork
      updateTriple (iriRef expression) (frbr .:. "part") musicExpr
      updateTriple musicWork (frbr .:. "adaption") (iriRef work)
      updateTriple musicExpr (frbr .:. "adaption") (iriRef expression)
      update

musicalWorkAdd expression work False = do
      list_ <- prefixes
      let [owl,rdf',rdfs,xsd,foaf,frbr,dce,dct,unic,unia,fabio,mo] = list_
      updateTriple (iriRef expression) (unia .:. "representsMusic") False
      update

videoAdd :: (SubjectTermLike a, ObjectTermLike a) => FilePath -> Maybe FilePath -> Config -> VideoJSONInfo -> a -> Query UpdateQuery
videoAdd basename separateImage cfg info uploader = do
      list_ <- prefixes
      let [owl,rdf',rdfs,xsd,foaf,frbr,dce,dct,unic,unia,fabio,mo] = list_
      let videoWork = unia .:. (encodeText (title info) <> "-" <> id_ info <> "-work")
      let film = unia .:. (encodeText (title info) <> "-" <> id_ info <> "-expr")
      let videoStream = iriRef $ "https://youtu.be/" <> id_ info --video_uri
      let videoFile = unia .:. (encodeText (title info) <> "-" <> id_ info <> "-man")
      let actualVideoFile = iriRef (archivePrefix cfg <> encodePath (T.pack $ youtubeFolder cfg </> basename))

      let thumbnailWork = unia .:. (encodeText (title info) <> "-" <> id_ info <> "-thumbnail-work")
      let thumbnailImage = unia .:. (encodeText (title info) <> "-" <> id_ info <> "-thumbnail-expr")
      let thumbnailImageFile = unia .:. (encodeText (title info) <> "-" <> id_ info <> "-thumbnail-man")
      let thumbnailRemote = iriRef $ thumbnail info
      let actualThumbnailFile = maybe actualVideoFile (iriRef . (archivePrefix cfg <>) . encodePath . T.pack) separateImage

      updateTriple videoWork rType (frbr .:. "Work")
      updateTriple videoWork (frbr .:. "realization") film
      updateTriple videoWork rType (fabio .:. "MovingImage")
      updateTriple videoWork (dce .:. "title") (title info)

      updateTriple film rType (frbr .:. "Expression")
      updateTriple film rType (fabio .:. "Movie")
      updateTriple film (frbr .:. "embodiment") videoFile
      updateTriple film (frbr .:. "embodiment") videoStream
      updateTriple film (dce .:. "title") (title info)
      whenJust (language info) (\lang -> updateTriple_ film (dce .:. "language") (lang, xsd .:. "language"))

      updateTriple videoStream rType (frbr .:. "Manifestation")
      updateTriple videoStream rType (fabio .:. "WebManifestation")
      updateTriple videoStream (frbr .:. "producer") uploader
      updateTriple videoStream (frbr .:. "reproduction") videoFile
      updateTriple videoStream (dce .:. "date") (dateFormatToDate $ upload_date info, xsd .:. "dateTime")
      updateTriple videoStream (dce .:. "title") (title info)
      updateTriple videoStream (dce .:. "description") (description info)
      updateTriple videoStream (dct .:. "extent") (duration info)

      updateTriple videoFile rType (frbr .:. "Manifestation")
      updateTriple videoFile rType (fabio .:. "DigitalManifestation")
      updateTriple videoFile (frbr .:. "exemplar") actualVideoFile
      updateTriple videoFile (dce .:. "title") (title info)
      updateTriple videoFile (dce .:. "description") (description info)
      updateTriple videoFile (dce .:. "date") (epochToXSDDateTime $ epoch info, xsd .:. "dateTime")
      updateTriple videoFile (dct .:. "extent") (duration info)

      updateTriple actualVideoFile rType (frbr .:. "Item")
      updateTriple actualVideoFile rType (fabio .:. "ComputerFile")
      updateTriple actualVideoFile (frbr .:. "owner") (unic .:. "me")

      updateTriple thumbnailWork rType (frbr .:. "Work")
      updateTriple thumbnailWork (frbr .:. "realization") thumbnailImage
      updateTriple thumbnailWork rType (fabio .:. "StillImage")

      updateTriple thumbnailImage rType (frbr .:. "Expression")
      updateTriple thumbnailImage rType (fabio .:. "Cover")
      updateTriple thumbnailImage (frbr .:. "complement") film
      updateTriple thumbnailImage (frbr .:. "embodiment") thumbnailImageFile
      updateTriple thumbnailImage (frbr .:. "embodiment") thumbnailRemote

      updateTriple thumbnailImageFile rType (frbr .:. "Manifestation")
      updateTriple thumbnailImageFile rType (fabio .:. "DigitalManifestation")
      updateTriple thumbnailImageFile (dce .:. "date") (epochToXSDDateTime $ epoch info, xsd .:. "dateTime")
      updateTriple thumbnailImageFile (frbr .:. "exemplar") actualThumbnailFile

      updateTriple thumbnailRemote rType (frbr .:. "Manifestation")
      updateTriple thumbnailRemote (frbr .:. "producer") uploader
      updateTriple thumbnailRemote (frbr .:. "reproduction") videoFile
      updateTriple thumbnailRemote (dce .:. "date") (dateFormatToDate $ upload_date info, xsd .:. "dateTime")
      updateTriple thumbnailRemote rType (fabio .:. "WebManifestation")

      updateTriple actualThumbnailFile rType (frbr .:. "Item")
      updateTriple actualThumbnailFile rType (fabio .:. "ComputerFile")
      updateTriple actualThumbnailFile (frbr .:. "owner") (unic .:. "me")

      update
instance SubjectTermLike RDF.Node

runQueries :: Config -> Bool -> FilePath -> IO Bool
runQueries cfg redo basename = do
  let json_file = mountPoint cfg </> youtubeFolder cfg </> basename -<.> "info.json"
  jsonReal <- doesFileExist json_file
  if jsonReal then do
    let jpeg = youtubeFolder cfg </> basename -<.> "jpg"
    let webp = youtubeFolder cfg </> basename -<.> "webp"
    jpegEx <- doesFileExist $ mountPoint cfg </> jpeg
    webpEx <- doesFileExist $ mountPoint cfg </> webp
    let image = if jpegEx then Just jpeg else if webpEx then Just webp else Nothing
    decoded <- decodeFileStrict json_file :: IO (Maybe VideoJSONInfo)
    case decoded of
      Nothing -> (putStrLn ("JSON " ++ json_file ++ " malformed!")) >> return False
      Just metadata -> do
        upload1 <- channelExistsSelect' cfg (channel_url metadata)
        --putStrLn $ createUpdateQuery $ uploaderAdd metadata
        upload <- maybe (uploaderAdd' cfg metadata >> putStrLn "Adding uploader..." >> channelExistsSelect' cfg (channel_url metadata) >>= maybe (error ("Retrieving uploader " ++ show metadata ++ " failed after adding")) return) return upload1
        --putStrLn "done uploader"
        alreadyThere <- askQuery (selectEndpoint cfg) $ isEmbodiedAsk $ "https://youtu.be/" <> id_ metadata
        if not redo && alreadyThere then putStrLn (basename ++ ": Already exists!") else do
          --putStrLn "doing video"
          --putStrLn $ createUpdateQuery $ videoAdd basename image cfg metadata upload
          res <- updateQuery (updateEndpoint cfg) $ videoAdd basename image cfg metadata upload
          if res then pure () else error ("Adding video info for " ++ show metadata ++ " failed!")
          putStrLn $ "Successfully added: " ++ basename
          when (jpegEx || webpEx) $ putStrLn "Added outside thumbnail!"
        return True
  else putStrLn "json does not exist!" >> return False

data YesNoSkip = Yes | No | Skip deriving (Eq, Show)

instance ToStylizedText YesNoSkip where
  toStylizedText Yes = bg green $ text "Yes"
  toStylizedText No = bg green $ text "No"
  toStylizedText Skip = bg green $ text "Skip"

addMusic :: Config -> IO ()
addMusic cfg = do
  notYetDone <- exprWorkSelect' cfg
  putStr "Remaining: "
  print $ Prelude.length notYetDone
  forM_ notYetDone $
    (\(expr,work,man,acc,title,name,length) -> runBylineT $ do
        sayLn $ text $ "Title: " <> title
        sayLn $ text $ "Uploader: " <> name
        sayLn $ text $ "Uploader URL: " <> acc
        sayLn $ text $ "Length: " <> length
        sayLn $ text $ "Link: " <> man
        res <- maybe (askWithMenuRepeatedly (menu (Skip :| [Yes, No])) (text "Is this music? ") (text "Try again")) pure $
          (if acc `Prelude.elem` musicUploaders cfg then Just Yes
                 else if acc `Prelude.elem` nonMusicUploaders cfg then Just No else Nothing)

        case res of
          Yes -> lift $ updateQuery (updateEndpoint cfg) $ musicalWorkAdd expr work True
          No -> lift $ updateQuery (updateEndpoint cfg) $ musicalWorkAdd expr work False
          Skip -> return True)

{-
artistAddByName :: Config -> TrackJSONInfo -> IO RDF.Node
artistAddByName cfg info = do
  r <- Y.channelExistsSelect' cfg (uploader_url info)
  case r of
    Nothing -> runBylineT (uploaderAdd'' (uploader info)) >>= (maybe (error "Couldn't choose uploader!") (pure . RDF.UNode))
    Just iri -> pure iri
  where artistAdd name = do
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
-}
