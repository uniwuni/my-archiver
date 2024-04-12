-- |
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Term where

import GHC.Generics
import Data.Aeson
import System.FilePath
--import Data.RDF hiding (Query, triple)
--import Data.Text
import qualified Data.Text as T
import Data.Text (Text)
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator
import qualified Data.RDF as RDF
import Control.Applicative ((<|>))
import qualified Data.RDF.Types as RDF
import Control.Monad (join, when)
import Data.Either
import Data.Maybe
import System.Directory
import YoutubeArchiver
import Byline.Menu
import Config
import Abbrevs
import Control.Monad.IO.Class
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List as L
instance ToStylizedText RDF.Node where
  toStylizedText (RDF.UNode x) = fg blue $ text ("<" <> x <> ">")
  toStylizedText (RDF.BNode x) = bg yellow $ text x
  toStylizedText (RDF.BNodeGen x) = bg yellow $ text (T.pack $ show x)
  toStylizedText (RDF.LNode (RDF.PlainL x)) = text x
  toStylizedText (RDF.LNode (RDF.PlainLL x lang)) = text x <> "@" <> (fg red $ text lang)
  toStylizedText (RDF.LNode (RDF.TypedL x typ)) = text x <> "^^" <> (fg blue $ text typ)

instance ToStylizedText BindingValue where
  toStylizedText (Bound x) = toStylizedText x
  toStylizedText Unbound = bg red $ text "unbound"

instance ToStylizedText [BindingValue] where
  toStylizedText xs = mconcat $ L.intersperse (text " | ")$ toStylizedText <$> xs

agentSelect :: Text -> Query SelectQuery
agentSelect name = do
  list_ <- prefixes
  let [owl,rdf',rdfs,xsd,foaf,frbr,dce,dct,unic,unia,fabio, mo] = list_
  agent <- var
  location <- var
  uri <- var
  name' <- var
  triple agent (foaf .:. "name") name'
  filterExpr (contains (lcase name') (lcase name) .||. contains (lcase name) (lcase name'))
  triple_ agent (rdf' .:. "type") (foaf .:. "Agent")
  selectVars [agent, name']

data AskResults a = OtherQuery | MakeNew | Result a deriving (Eq, Ord, Show)

instance ToStylizedText a => ToStylizedText (AskResults a) where
  toStylizedText OtherQuery = (bg blue $ text "*Other Query* ")
  toStylizedText MakeNew = (bg yellow $ text "*Make New* ")
  toStylizedText (Result x) = toStylizedText x


chooseFromSelect :: Config -> Query SelectQuery -> Stylized Text -> BylineT IO (AskResults (Choice [BindingValue]))
chooseFromSelect cfg query prompt = do
  possibilities <- liftIO $ selectQuery (selectEndpoint cfg) query
  case possibilities of
    Just x@(a@(_:_):as) -> do
      process <$> askWithMenu (menu $ OtherQuery :| MakeNew : (Result <$> x)) prompt
    a -> process <$> askWithMenu (menu (OtherQuery:|[MakeNew :: AskResults [BindingValue]])) prompt
  where choiceFunc menu map text =
          if T.strip text `elem` ["new","n"] then MakeNew else
            if T.strip text `elem` ["query","q"] then OtherQuery else
              Result $ defaultFromChoice menu map text
        process (Match OtherQuery) = OtherQuery
        process (Match MakeNew) = MakeNew
        process (Match (Result a)) = Result (Match a)
        process (Other text) = Result (Other text)
