module Main where

import Control.Monad
import System.Directory
import Pdf.Document
import DocReader
import BM25
import Utils

-- Obs: you gotta use ':set -package text' and ':set -package directory' before loading

query :: [Token] = tokenizer ("partial")
filepath :: String = "../../data/subset/"

main = do
       ---- Gathering docs
       files <- getDirectoryContents filepath
       let pdf_names = filter (is_file_type "pdf") (map (filepath ++) files)

       ---- Processing docs
       not_io_documents <- mapM tokenizeDoc pdf_names

       ---- Filtering sucessfully processed docs
       let documents = filter (not_empty . snd) not_io_documents :: [(String, [String])]
       let doc_contents = map snd documents
       let n_total_docs = length pdf_names
       let n_processed_docs = length documents
       putStrLn $ "Processed " ++ (show n_processed_docs) ++ " out of " ++ (show n_total_docs)

       ---- Calculating most relevant doc for a given query
       -- parameters
       let nDocs :: Double = fromIntegral n_processed_docs
       let avgdl :: Double = get_avgdl nDocs doc_contents
       -- result
       putStrLn $ fst $ get_most_relevant_doc (nDocs, avgdl) documents doc_contents query

