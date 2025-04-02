module Main where

import Control.Monad
import System.Directory
import Pdf.Document
import DocReader
import BM25
import Utils

-- Obs: you gotta use ':set -package directory' before loading

query :: [Token] = tokenizer ("query")
filepath :: String = "../data/subset/"

main = do
       files <- getDirectoryContents filepath
       -- processing docs
       let pdf_names = filter (is_file_type "pdf") (map (filepath ++) files)
       not_io_documents <- mapM tokenizeDoc pdf_names
       let documents = filter (not_empty . snd) not_io_documents :: [(String, [String])]
       let docs_contents = map snd documents
       let n_total_docs = length pdf_names
       let n_processed_docs = length documents
       putStrLn $ "Processed " ++ (show n_processed_docs) ++ " out of " ++ (show n_total_docs)
       -- parameters
       let nDocs :: Double = fromIntegral (length documents)
       let avgdl :: Double = get_avgdl nDocs docs_contents
       -- result
       putStrLn $ fst $ get_most_relevant_doc (nDocs, avgdl) documents docs_contents query

