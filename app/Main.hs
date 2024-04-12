{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified YoutubeArchiver as Y
import qualified SoundcloudArchiver as S
import System.Console.CmdArgs.Explicit
import Data.Aeson
import System.FilePath
import Control.Monad
import Config
import Data.Maybe
command :: Mode [(String,String)]
command = modes "RDF tool" [] "Convert YouTube/SoundCloud archive data to RDF"
  [mode "youtube" [("youtube", "youtube")]
   "YouTube archive conversion" (flagArg (\x y -> Right y) "No argument") flags,
  mode "soundcloud" [("soundcloud","soundcloud")]
   "SoundCloud archive conversion" (flagArg (\x y -> Right y) "No argument") flags,
  mode "checkMusic" [("checkMusic","checkMusic")]
   "Manually tag whether something is music" (flagArg (\x y -> Right y) "No argument") [configFlag]]

  where upd name x v = Right $ (name,x):v
        flags = [configFlag,
                 flagReq ["video","v"] (upd "video") "VIDEO_FILE" "video to process",
                 flagReq ["list","l"] (upd "list") "LIST_FILE" "list of paths of videos to process",
                 flagNone ["redo","r"] (("redo", "redo"):) "Redo all videos",
                 flagHelpSimple (("help",""):)]
        configFlag = flagReq ["config","c"] (upd "config") "CONFIG_FILE" "config file"

main :: IO ()
main = do
  as <- processArgs command
  if ("help","") `elem` as then
           print $ helpText [] HelpFormatDefault command
  else do
    let querier = if isJust $ lookup "youtube" as then Y.runQueries else if isJust $ lookup "soundcloud" as then S.runQueries else error "no mode specified!"
    let Just cfg_file = lookup "config" as
    Just config <- (decodeFileStrict cfg_file :: IO (Maybe Config))
    if isJust $ lookup "checkMusic" as then Y.addMusic config else

      case lookup "video" as of
        Just video_file -> do
          querier config (("redo", "redo") `elem` as) video_file
          return ()
        Nothing -> do
          let Just list_file = lookup "list" as
          l <- lines <$> readFile list_file
          forM_ l $ querier config (("redo", "redo") `elem` as)
