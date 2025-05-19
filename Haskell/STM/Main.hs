module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM.TVar
import System.Directory
import Pdf.Document
import DocReader
import BM25
import Utils

-- Obs: you gotta use ':set -package text',
-- ':set -package directory' and ':set -package stm' before loading

query = tokenizer "partial function" :: [Token]
filepath = "../../data/haskell_subset/"
n_threads = 13

main = do
       doc_score_list <- calculate_score_list (process_docs gather_docs)
       print $ fst (get_most_relevant_doc doc_score_list)

gather_docs :: IO [String]
gather_docs = do
       files <- getDirectoryContents filepath
       let pdf_names = filter (is_file_type "pdf") (map (filepath ++) files)
       return pdf_names

process_docs :: IO [String] -> IO [DocumentData]
process_docs input = do
       pdf_names <- input

       let n_pdf_names = length pdf_names

       tvar_doc_data <- create_emtpy_tvars n_pdf_names (DocumentData{
                                                        name="unread",
                                                        n_tokens=0,
                                                        token_freq=[]
                                                        }) :: IO [TVar DocumentData]
       thread_prints <- create_emtpy_tvars n_pdf_names ""

       -- dividing work between n_threads
       let process_function = mapM $ wrapper (get_doc_data query) tokenizeDoc
       let n_threads_input = split_into_n_lists n_threads (zip tvar_doc_data (zip thread_prints pdf_names))
       threadIds <- mapM (fork_aux forkIO process_function) n_threads_input

       putStrLn $ "Processing docs with: " ++ (show $ length threadIds) ++ " threads."

       -- waits until all threads finish
       mapM printTVar thread_prints
       doc_data_list <- list_of_io_to_io_list $ mapM atomReadWait tvar_doc_data (DocumentData{
                                                        name="unread",
                                                        n_tokens=0,
                                                        token_freq=[]
                                                        }) :: IO [DocumentData]

       -- Filtering out empty docs
       let documents = filter (not_empty . name) doc_data_list :: [DocumentData]

       putStrLn $ "Processed " ++ (show $ length documents) ++ " out of " ++ (show $ n_pdf_names)
       return documents

calculate_score_list :: IO [DocumentData] -> IO [(String, Double)]
calculate_score_list input = do
       documents <- input

       let n_processed_docs = length documents
       let nDocs = fromIntegral n_processed_docs :: Double
       let avgdl = get_avgdl nDocs (map n_tokens documents) :: Double
       let idfs = iDF nDocs documents query

       -- dividing work between n_threads
       doc_score_tvars <- create_emtpy_tvars n_processed_docs ("", 0) :: IO [TVar (String, Double)]
       let n_threads_input = split_into_n_lists n_threads (zip doc_score_tvars documents)
       threadIds <- mapM (fork_aux forkOS 
              (mapM $ multithread_doc_score (nDocs, avgdl, idfs) query)) n_threads_input
       
       putStrLn $ "Calculating Score with: " ++ (show $ length threadIds) ++ " threads."

       -- waits until all threads finish
       tvar_list_to_list_wait doc_score_tvars ("", 0)