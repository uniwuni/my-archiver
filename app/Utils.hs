-- |
{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.Clock
import qualified Network.URI.Encode as NURI
import qualified Data.Text as T
import Data.Text
import Data.RDF.Types

showLValue :: LValue -> Text
showLValue (PlainL t) = t
showLValue (PlainLL t l) = t <> "@" <> l
showLValue (TypedL t ty) = t <> "^^" <> ty


encodePath, encodeText :: Text -> Text
encodePath = NURI.encodeTextWith (\x -> (NURI.isAllowed x || x == '/') && x /= '~')
encodeText = NURI.encodeTextWith (\x -> NURI.isAllowed x && x /= '~')

epochToXSDDateTime :: Integer -> Text
epochToXSDDateTime epoch = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" utcTime
  where utcTime = posixSecondsToUTCTime $ realToFrac epoch
--mport Data.RDF.IRI

-- | When the given value is of "Just a" form, execute the given action,
-- otherwise do nothing.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just v) f = f v
whenJust Nothing _ = return ()
