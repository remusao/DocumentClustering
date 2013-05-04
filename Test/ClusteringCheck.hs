
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
                    clustering <- clusterDocumentsVerbose (KMeans 4) Cosine $ zip args documents
                    print clustering
