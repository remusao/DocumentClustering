{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module NLP.TextClustering
    (Algorithm(..),
     Distance(..),
     clusterDocuments,
     clusterDocumentsVerbose)
where

import Math.IDF
import NLP.TextClustering.DistanceUtils
import Math.HKMeans.KMeans (kmeans)
import Numeric.LinearAlgebra (Vector, dim)
import Data.Char (toLower)
import NLP.Tokenize (tokenize)
import Data.Ord (comparing)
import Data.List (sortBy)


-- Option of the clusterization
data Algorithm a where
    KMeans :: Int -> Algorithm Int
data Distance where
    Euclidean :: Distance
    Cosine :: Distance

deriving instance Show a => Show (Algorithm a)
deriving instance Show Distance

-- Types to make the code cleaner
type Text           = String
type Name           = String
type Word           = String
type Document       = (Name, Text)


-- Part Of Speech Tagging
type Tag = Int
partOfSpeechTagging :: [Word] -> [(Tag, Word)]
partOfSpeechTagging = zip [1..] -- Just to make it compile


-- Filter Useless Words
stopWords :: [Word]
stopWords = ["a", "able", "about", "across", "after", "all", "almost", "also", "am", "among", "an", "and", "any", "are", "as", "at", "be", "because", "been", "but", "by", "can", "cannot", "could", "dear", "did", "do", "does", "either", "else", "ever", "every", "for", "from", "get", "got", "had", "has", "have", "he", "her", "hers", "him", "his", "how", "however", "i", "if", "in", "into", "is", "it", "its", "just", "least", "let", "like", "likely", "may", "me", "might", "most", "must", "my", "neither", "no", "nor", "not", "of", "off", "often", "on", "only", "or", "other", "our", "own", "rather", "said", "say", "says", "she", "should", "since", "so", "some", "than", "that", "the", "their", "them", "then", "there", "these", "they", "this", "tis", "to", "too", "twas", "us", "wants", "was", "we", "were", "what", "when", "where", "which", "while", "who", "whom", "why", "will", "with", "would", "yet", "you", "your"]

filterWords :: [(Tag, Word)] -> [(Tag, Word)]
filterWords = filter (\w -> (notElem (snd w) stopWords) && (length . snd) w > 3)



-- Lemmatize
lemmatize :: [(Tag, Word)] -> [Word]
lemmatize = snd . unzip


-- Clusterize
clusterize :: Algorithm a -> Distance -> [Vector Double] -> [Int]
clusterize algorithm distance =
    case algorithm of
        (KMeans k) -> kmeans d k
    where
        -- Select the distance to use
        d = case distance of
                Euclidean -> euclideanDistance
                Cosine -> cosineDistance



clusterDocuments :: Algorithm a -> Distance -> [Document] -> [(Name, Int)]
clusterDocuments algorithm distance documents =
    let (names, texts) = unzip documents -- Separate names from their respective text
        preprocessing = map $       -- Apply preprocessing on texts
              lemmatize             -- Lemmatize each word to a canonical form
            . filterWords           -- Filter useless and common words
            . partOfSpeechTagging   -- Find part of speech tag for each word
            . tokenize              -- Tokenize the document
            . map toLower           -- Lower every letters in the document
        datas = idf $ preprocessing texts -- Build feature space
    in sortBy (comparing snd) $ zip names $ clusterize algorithm distance datas


clusterDocumentsVerbose :: Show a => Algorithm a -> Distance -> [Document] -> IO ([(Name, Int)])
clusterDocumentsVerbose algorithm distance documents =
    do
        let (names, texts) = unzip documents -- Separate names from their respective text
            preprocessing = map $       -- Apply preprocessing on texts
                  lemmatize             -- Lemmatize each word to a canonical form
                . filterWords           -- Filter useless and common words
                . partOfSpeechTagging   -- Find part of speech tag for each word
                . tokenize              -- Tokenize the document
                . map toLower           -- Lower every letters in the document
            datas = idf $ preprocessing texts -- Build feature space
            res = sortBy (comparing snd) $ zip names $ clusterize algorithm distance datas
        putStrLn $ "Debug trace of clustering"
        putStrLn $ "Distance : " ++ (show distance)
        putStrLn $ "Clustering : " ++ (show algorithm)
        putStrLn $ "Dimension of feature space : " ++ (show $ dim $ head datas)
        print $ datas
        return res
