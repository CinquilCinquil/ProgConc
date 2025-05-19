module Main where

import Control.Monad
import System.Directory
import Pdf.Document
import DocReader
import BM25
import Utils

-- Obs: you gotta use ':set -package text' and ':set -package directory' before loading

query = tokenizer "partial" :: [Token]
filepath = "../../data/haskell_subset/" :: String

main = do
       ---- Gathering docs
       files <- getDirectoryContents filepath
       let pdf_names = filter (is_file_type "pdf") (map (filepath ++) files)

       ---- Processing docs
       doc_data_list <- mapM (get_doc_data query . tokenizeDoc) pdf_names :: IO [DocumentData]

       let documents = filter (not_empty . name) doc_data_list :: [DocumentData]
       let n_processed_docs = length documents

       putStrLn $ "Processed " ++ (show n_processed_docs) ++ " out of " ++ (show $ length pdf_names)

       ---- Calculating Score

       let nDocs = fromIntegral n_processed_docs :: Double
       let avgdl = get_avgdl nDocs (map n_tokens doc_data_list) :: Double
       let idfs = iDF nDocs doc_data_list query

       putStrLn $ fst $ get_most_relevant_doc (nDocs, avgdl, idfs) doc_data_list query
