-- |
{-# LANGUAGE OverloadedStrings #-}
module Abbrevs where
import Database.HSparql.QueryGenerator
import qualified Data.Text as T

rdfPrefix = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
frbrPrefix = "http://purl.org/vocab/frbr/core#"
rType = iriRef $ rdfPrefix <> "type"

prefixes :: Query [Prefix]
prefixes = sequence
  [prefix "owl" (iriRef "http://www.w3.org/2002/07/owl#"),
   prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
   prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#"),
   prefix "xsd" (iriRef "http://www.w3.org/2001/XMLSchema#"),
   prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/"),
   prefix "frbr" (iriRef "http://purl.org/vocab/frbr/core#"),
   prefix "dce" (iriRef "http://purl.org/dc/elements/1.1/"),
   prefix "dct" (iriRef "http://purl.org/dc/terms/"),
   prefix "unic" (iriRef "https://uniwuni.github.io/me#"),
   prefix "unia" (iriRef "https://uniwuni.github.io/archives#"),
   prefix "fabio" (iriRef "http://purl.org/spar/fabio/"),
   prefix "mo" (iriRef "http://purl.org/ontology/mo/")]
