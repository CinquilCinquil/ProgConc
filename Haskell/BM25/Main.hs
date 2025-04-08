module Main where

import Control.Monad
import Control.Concurrent
import System.Directory
import Pdf.Document
import DocReader
import BM25
import Utils

-- Obs: you gotta use "":set -package text' and ':set -package directory' before loading

query :: [Token] = tokenizer ("partial")
filepath :: String = "../../data/subset/"

main = do
       files <- getDirectoryContents filepath
       let pdf_names = filter (is_file_type "pdf") (map (filepath ++) files)
       -- processing docs
       mvar_documents <- create_emtpy_vars (length pdf_names)
       let input_list = zip (pdf_names, mvar_documents)
       threadsIds <- mapM (forkIO . tokenizeDoc) input_list

       non_mvar_documents <- mapM takeMVar mvar_documents -- waits until all threads finish
       let documents = filter (not_empty . snd) non_mvar_documents :: [NameAndDoc]
       let docs_contents = map snd documents
       let n_total_docs = length pdf_names
       let n_processed_docs = length documents
       putStrLn $ "Processed " ++ (show n_processed_docs) ++ " out of " ++ (show n_total_docs)
       -- parameters
       let nDocs :: Double = fromIntegral (length documents)
       let avgdl :: Double = get_avgdl nDocs docs_contents
       -- result
       putStrLn $ fst $ get_most_relevant_doc (nDocs, avgdl) documents docs_contents query  -- parallel this

create_emtpy_vars :: Int -> IO [MVar NameAndDoc]
create_emtpy_vars 0 = return []
create_emtpy_vars n = do
       new_empty_mvar <- newEmptyMVar :: IO (MVar NameAndDoc)
       list <- create_emtpy_vars (n - 1)
       let return_list = (new_empty_mvar):(list) :: [MVar NameAndDoc]
       return return_list