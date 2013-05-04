
module NLP.TextClustering.FeatureSpace(buildFeatureVectors) where

import Numeric.LinearAlgebra
import Data.Map (Map)
import qualified Data.Map as Map


type Word = String

-- Count the words in a document
countWords :: [Word] -> Map String Double -> Map String Double
countWords [] m = m
countWords (w:t) m = countWords t $ updateInsert w
    where
        updateInsert word =
            case Map.lookup word m of
                Nothing -> Map.insert word 1 m
                Just _ -> Map.adjust (+ 1) word m


-- Count all occurences of all words in all documents
countAll :: [[Word]] -> Map String Double -> Map String Double
countAll [] m = m
countAll (d:t) m = countAll t $ countWords d m


-- Compute the feature vector associated with each document
buildVec :: Map String Double -> Int -> [Word] -> Vector Double
buildVec m d doc =
    let count = countWords doc $ Map.empty
    in  fromList $ map (computeCoeff count) $ Map.keys m
    where
        computeCoeff count w =
            case Map.lookup w count of
                Nothing -> 0.0
                Just val -> val / (fromIntegral d)


buildFeatureVectors :: [[Word]] -> [Vector Double]
buildFeatureVectors documents =
    let count = countAll documents $ Map.empty :: Map String Double
    in map (buildVec count $ Map.size count) documents
