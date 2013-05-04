
module Main where

import System.Environment
import NLP.TextClustering

usage :: IO ()
usage = putStrLn $
       "ClusteringCheck   :: usage\n"
    ++ "--debug [texts]   :: Print debug informations\n"
    ++ "--normal [texts]  :: Only print resulting cluster"

main :: IO ()
main =
    do
        (opt:args) <- getArgs
        case length args of
            0 -> error "No document given"
            _ ->
                case opt of
                    "--debug" ->
                        do
                            documents <- mapM readFile args
                            clustering <- clusterDocumentsVerbose (KMeans 4) Cosine $ zip args documents
                            print clustering
                    "--normal" ->
                        do
                            documents <- mapM readFile args
                            print $ clusterDocuments (KMeans 4) Cosine $ zip args documents
                    _ -> usage
