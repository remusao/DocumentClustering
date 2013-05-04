
module Main where

import System.Environment
import NLP.TextClustering

main :: IO ()
main =
    do
        args <- getArgs
        case length args of
            0 -> error "No document given"
            _ ->
                do
                    documents <- mapM readFile args
                    print $ clusterDocuments (KMeans 2) Euclidean $ zip args documents
