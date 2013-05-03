
module NLP.TextClustering.DistanceUtils
    (cosineDistance
    ,euclideanDistance)
where

import Numeric.LinearAlgebra

-- Cosine distance
cosineDistance :: Vector Double -> Vector Double -> Double
cosineDistance d1 d2 = (d1 <.> d2) / (norm1 d1) * (norm1 d2)


-- Euclidian distance
euclideanDistance :: Vector Double -> Vector Double -> Double
euclideanDistance d1 d2 = norm1 (d1 - d2)
