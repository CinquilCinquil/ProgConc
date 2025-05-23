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
test_filepath = "../../data/tests/CUDA Thread-Indexing Cheatsheet.pdf" :: String
test_text = "Lorem ipsum dolor sit amet, consectetur\n adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation\n ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat\n cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
test_docdata = DocumentData {name = "test", n_tokens = 3, token_freq = [("in", 10), ("out", 6), ("idk", 3)]}
test_tokens = tokenizer test_text
test_io_data = do return ("test", test_tokens)
test_documents = [
        DocumentData {name = "test1", n_tokens = 3, token_freq = [("in", 10), ("out", 6), ("idk", 3)]},
        DocumentData {name = "test2", n_tokens = 3, token_freq = [("in", 0), ("out", 1), ("idk", 20)]},
        DocumentData {name = "test3", n_tokens = 3, token_freq = [("in", 1), ("out", 3), ("idk", 7)]}
    ]

avgdl = 3
nDocs = 3
test_params = (nDocs, avgdl, iDF 3 test_documents test_query)
test_scores = [("a", 10.2), ("b", 4.3), ("c", 69)]

create_non_emtpy_mvars :: Int -> IO [MVar Int]
create_non_emtpy_mvars 0 = return []
create_non_emtpy_mvars n = do
       new_mvar <- newMVar 42
       list <- create_non_emtpy_mvars (n - 1)
       let return_list = (new_mvar):(list)
       return return_list

main = do

    let tokenizeDocBench = perRunEnv (newEmptyMVar :: IO (MVar String)) $ \mv -> tokenizeDoc mv test_filepath
    let get_doc_dataBench = perRunEnv (newEmptyMVar :: IO (MVar DocumentData)) $ \mv -> get_doc_data test_query mv test_io_data
    let multithread_doc_scoreBench = perRunEnv (newEmptyMVar :: IO (MVar (String, Double))) $ \mv -> multithread_doc_score test_params test_query (mv, test_docdata)
    let mvar_list_to_listBench = perRunEnv (create_non_emtpy_mvars 5) $ \mvs -> mvar_list_to_list mvs

    defaultMain [
        bgroup "DocReader" [
        bench "tokenizer" $ nf tokenizer test_text,
        --bench "tokenizeDoc" $ tokenizeDocBench,
        bench "get_token_freq" $ nf (get_token_freq "out") test_docdata,
        bench "get_doc_data" $ get_doc_dataBench,
        bench "amount_of_documents_with" $ nf (amount_of_documents_with "in") test_documents
        ],
        bgroup "BM25" [
        bench "get_most_relevant_doc" $ nf get_most_relevant_doc test_scores,
        bench "multithread_doc_score" $ multithread_doc_scoreBench,
        bench "get_avgdl" $ nf (get_avgdl nDocs) [1000, 555, 431], -- TODO: increase? (on the others as well)
        bench "iDF" $ nf (iDF nDocs test_documents) test_query,
        bench "doc_score" $ nf (doc_score test_params test_docdata) test_query
        ],
        bgroup "Utils" [
        bench "clean_str" $ nf clean_str test_text,
        bench "token_frequency" $ nf (token_frequency test_tokens) "in",
        bench "is_file_type" $ nf (is_file_type test_filepath) "pdf",
        bench "conct_to_head" $ nf (conct_to_head 'a') ["bc", "def"],
        bench "fits_at_ys_start" $ nf (fits_at_ys_start "abc") "abcdef",
        bench "remove_str" $ nf (remove_str "de") "abcdef",
        bench "remove_sequence_of_str" $ nf (remove_sequence_of_str ["b", "de"]) "abcdef",
        bench "split_into_n_lists" $ nf (split_into_n_lists 3) ["1", "2", "3", "4", "5", "6", "7"],
        bench "create_emtpy_mvars" $ nfIO (create_emtpy_mvars 5),
        bench "mvar_list_to_list" $ mvar_list_to_listBench
        ]]

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
       threadIds <- mapM (fork_aux forkOS 
              (mapM $ multithread_doc_score (nDocs, avgdl, idfs) test_query)) n_threads_input
       
       putStrLn $ "Calculating Score with: " ++ (show $ length threadIds) ++ " threads."

       -- waits until all threads finish
       mvar_list_to_list doc_score_mvars

-- use this to do a Macrobenchmark
--main = do defaultMain [bench "mainBench" $ nfIO mainBench]