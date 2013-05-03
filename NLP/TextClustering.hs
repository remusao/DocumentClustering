

module NLP.TextClustering where

import Data.Char (toLower)
import NLP.Tokenize

type Text           = String
type Name           = String
type Document       = (Name, Text)
type TaggedDocument = (Int, Name)

-- Part Of Speech Tagging
partOfSpeechTagging :: a -> a
partOfSpeechTagging = id

-- Filter Useless Words
filterWords :: a -> a
filterWords = id

-- Lemmatize
lemmatize :: a -> a
lemmatize = id

-- Statistics of documents
documentStatistics :: a -> a
documentStatistics = id

--clusterDocuments :: [Document] -> [TaggedDocument]
clusterDocuments :: [String] -> [[String]]
clusterDocuments = map $
    documentStatistics      -- Count words to build a vector for the document
    . lemmatize             -- Lematize each word to a canonical form
    . filterWords           -- Filter useless and common words
    . partOfSpeechTagging   -- Find part of speech tag for each word
    . tokenize              -- Tokenize the document
    . map toLower           -- Lower every letter in the document
