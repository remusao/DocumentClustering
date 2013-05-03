{-# LANGUAGE GADTs #-}

module NLP.TextClustering where

import NLP.TextClustering.DistanceUtils
import Math.HKMeans.KMeans
import Numeric.LinearAlgebra
import Data.Char (toLower)
import NLP.Tokenize


-- Option of the clusterization
data Algorithm where
    KMeans :: Int -> Algorithm
data Distance where
    Euclidean :: Distance
    Cosine :: Distance
    Custom :: (Vector Double -> Vector Double -> Double) -> Distance


-- Types to make the code cleaner
type Text           = String
type Name           = String
type Word           = String
type Document       = (Name, Text)
type TaggedDocument = (Int, Name)


-- Part Of Speech Tagging
type Tag = Int
partOfSpeechTagging :: [Word] -> [(Tag, Word)]
partOfSpeechTagging = zip [1..] -- Just to make it compile


-- Filter Useless Words
filterWords :: [(Tag, Word)] -> [(Tag, Word)]
filterWords = id


-- Lemmatize
lemmatize :: [(Tag, Word)] -> [Word]
lemmatize = snd . unzip


-- Statistics of documents
documentStatistics :: [Word] -> Vector Double
documentStatistics _ = fromList [1..42]


-- Clusterize
clusterize :: Algorithm -> Distance -> [Vector Double] -> [Int]
clusterize algorithm distance = cluster
    where
        -- Select the good clustering algorithm
        cluster =
            case algorithm of
                (KMeans k) -> kmeans d k
                _ -> error "No such clusterization algorithm"
        -- Select the distance to use
        d = case distance of
                Euclidean -> euclideanDistance
                Cosine -> cosineDistance
                (Custom _distance) -> _distance
                _ -> error "No Such distance"


clusterDocuments :: Algorithm -> Distance -> [Document] -> [(Int, Name)]
clusterDocuments algorithm distance documents =
    let (names, texts) = unzip documents
        preprocessing = map $
            documentStatistics      -- Count words to build a vector for the document
            . lemmatize             -- Lematize each word to a canonical form
            . filterWords           -- Filter useless and common words
            . partOfSpeechTagging   -- Find part of speech tag for each word
            . tokenize              -- Tokenize the document
            . map toLower           -- Lower every letters in the document
    in zip (clusterize algorithm distance (preprocessing texts)) names
