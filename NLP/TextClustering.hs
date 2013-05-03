{-# LANGUAGE GADTs #-}

module NLP.TextClustering where

import NLP.TextClustering.DistanceUtils
import Math.HKMeans.KMeans (kmeans)
import Numeric.LinearAlgebra (Vector, fromList)
import Data.Char (toLower)
import NLP.Tokenize (tokenize)
import Data.Ord (comparing)
import Data.List (sortBy)


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


-- Build the feature vectors representing the texts
buildFeatureVectors :: [[Word]] -> [Vector Double]
buildFeatureVectors _ = [fromList [1..42]]


-- Clusterize
clusterize :: Algorithm -> Distance -> [Vector Double] -> [Int]
clusterize algorithm distance =
    case algorithm of
        (KMeans k) -> kmeans d k
    where
        -- Select the distance to use
        d = case distance of
                Euclidean -> euclideanDistance
                Cosine -> cosineDistance
                (Custom _distance) -> _distance


clusterDocuments :: Algorithm -> Distance -> [Document] -> [(Name, Int)]
clusterDocuments algorithm distance documents =
    let (names, texts) = unzip documents -- Separate names from their respective text
        preprocessing = map $       -- Apply preprocessing on texts
              lemmatize             -- Lemmatize each word to a canonical form
            . filterWords           -- Filter useless and common words
            . partOfSpeechTagging   -- Find part of speech tag for each word
            . tokenize              -- Tokenize the document
            . map toLower           -- Lower every letters in the document
        datas = buildFeatureVectors $ preprocessing texts -- Build feature space
    in sortBy (comparing snd) $ zip names $ clusterize algorithm distance datas
