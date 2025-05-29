{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Criterion.Main
import System.Directory
import Control.DeepSeq (NFData(..))
import Control.Monad
import Control.Concurrent
import Data.Text (Text, unpack)
import Pdf.Document

import DocReader
import BM25
import Utils

instance NFData DocumentData where
  rnf (DocumentData {name, n_tokens, token_freq}) = 
    rnf name `seq` (rnf n_tokens `seq` rnf token_freq)

test_query = tokenizer "in out idk" :: [Token]
test_directory = "../../data/subset/"

--- copy of the main function with local variables
n_threads_doc = 12
n_threads_score = 12

mainBench = do
       doc_score_list <- calculate_score_list (process_docs gather_docs)
       print $ fst (get_most_relevant_doc doc_score_list)

gather_docs :: IO [String]
gather_docs = do
       files <- getDirectoryContents test_directory
       let pdf_names = filter (is_file_type "pdf") (map (test_directory ++) files)
       return pdf_names

process_docs :: IO [String] -> IO [DocumentData]
process_docs input = do
       pdf_names <- input

       let n_pdf_names = length pdf_names

       mvar_doc_data <- create_emtpy_mvars n_pdf_names :: IO [MVar DocumentData]
       thread_print <- newEmptyMVar

       -- dividing work between n_threads
       let process_function = mapM $ wrapper (get_doc_data test_query) (tokenizeDoc thread_print)
       let n_threads_input = split_into_n_lists n_threads_doc (zip mvar_doc_data pdf_names)
       threadIds <- mapM (fork_aux forkIO process_function) n_threads_input

       putStrLn $ "Processing docs with: " ++ (show $ length threadIds) ++ " threads."

       -- waits until all threads finish
       replicateM_ n_pdf_names (printMVar thread_print)
       doc_data_list <- mapM takeMVar mvar_doc_data :: IO [DocumentData]

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
       let idfs = iDF nDocs documents test_query

       -- dividing work between n_threads
       doc_score_mvars <- create_emtpy_mvars n_processed_docs :: IO [MVar (String, Double)]
       let n_threads_input = split_into_n_lists n_threads_score (zip doc_score_mvars documents)
       threadIds <- mapM (fork_aux forkIO
            (mapM $ multithread_doc_score (nDocs, avgdl, idfs) test_query)) n_threads_input
       
       putStrLn $ "Calculating Score with: " ++ (show $ length threadIds) ++ " threads."

       -- waits until all threads finish
       mvar_list_to_list doc_score_mvars

-- use this to do a Macrobenchmark
main = do defaultMain [bench "mainBench" $ nfIO mainBench]