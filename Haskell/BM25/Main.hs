module Main where

import Control.Monad
import Control.Concurrent
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
       let n_pdf_names = length pdf_names

       ---- Processing docs
       mvar_documents <- create_emtpy_mvars n_pdf_names
       thread_print <- newEmptyMVar

       let input_list = zip pdf_names mvar_documents

       -- Parsing each document on a separate thread
       threadIds <- mapM (forkIO . tokenizeDoc thread_print) input_list

       replicateM_ n_pdf_names (printMVar thread_print)
       -- waits until all threads finish
       non_mvar_documents <- mapM takeMVar mvar_documents

       ---- Filtering sucessfully processed docs
       let documents = filter (not_empty . snd) non_mvar_documents :: [NameAndDoc]
       let docs_contents = map snd documents
       let n_processed_docs = length documents
       putStrLn $ "Processed " ++ (show n_processed_docs) ++ " out of " ++ (show n_pdf_names)

       ---- Calculating most relevant doc for a given query
       -- parameters
       let nDocs :: Double = fromIntegral n_processed_docs
       let avgdl :: Double = get_avgdl nDocs docs_contents
       -- result
       putStrLn $ fst $ get_most_relevant_doc (nDocs, avgdl) documents docs_contents query  -- parallel this