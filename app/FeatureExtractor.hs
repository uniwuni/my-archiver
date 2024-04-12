-- |

module FeatureExtractor where


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
import YoutubeArchiver

type PreIRI = Text

makeArtist :: Text -> PreIRI -> [PreIRI] -> Query UpdateQuery
makeArtist name iri terms = do
  rdf' <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  rdfs' <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
  uni <- prefix "uni" (iriRef "http://uniwuni.github.io/archive-ontology#")
  addUploaderType <- updateTriple iri (rdf' .:. "type") (uni .:. "AbstractAgent")
  addName <- updateTriple iri (uni .:. "hasName") name
  map updateTriple iri (uni .:. "isAssociated") name
