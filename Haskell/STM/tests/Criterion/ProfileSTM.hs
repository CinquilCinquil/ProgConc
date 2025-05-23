{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Criterion.Main
import System.Directory
import Control.DeepSeq (NFData(..))
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Data.Text (Text, unpack)
import Pdf.Document

import DocReader
import BM25
import Utils

instance NFData DocumentData where
  rnf (DocumentData {name, n_tokens, token_freq}) = 
    rnf name `seq` (rnf n_tokens `seq` rnf token_freq)

instance NFData (TVar a) where
    rnf _ = ()

test_query = tokenizer "in out idk" :: [Token]
test_directory = "../../data/tests/"
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

create_non_emtpy_tvars :: Int -> IO [TVar Int]
create_non_emtpy_tvars 0 = return []
create_non_emtpy_tvars n = do
       new_tvar <- newTVarIO 42
       list <- create_non_emtpy_tvars (n - 1)
       let return_list = (new_tvar):(list)
       return return_list

main = do

    let tokenizeDocBench = perRunEnv (newTVarIO "") $ \mv -> tokenizeDoc (mv, test_filepath)
    let get_doc_dataBench = perRunEnv (newTVarIO test_docdata) $ \mv -> get_doc_data test_query mv test_io_data
    let multithread_doc_scoreBench = perRunEnv (newTVarIO ("", 0)) $ \mv -> multithread_doc_score test_params test_query (mv, test_docdata)
    let tvar_list_to_list_waitBench = perRunEnv (create_non_emtpy_tvars 5) $ \mvs -> tvar_list_to_list_wait mvs 0
    let printTVarBench = perRunEnv (newTVarIO "test") $ \mv -> printTVar mv
    let atomReadWaitBench = perRunEnv (newTVarIO "test") $ \mv -> atomReadWait mv ""

    test_tvar <- newTVarIO "test"

    defaultMain [
        bgroup "DocReader" [
        bench "tokenizer" $ nf tokenizer test_text,
        bench "tokenizeDoc" $ tokenizeDocBench,
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
        bench "create_emtpy_tvars" $ nfIO (create_emtpy_tvars 5 0),
        bench "tvar_list_to_list_wait" $ tvar_list_to_list_waitBench,
        bench "printTVar" $ printTVarBench,
        bench "atomReadWait" $ atomReadWaitBench,
        bench "atomRead" $ nfIO (atomRead test_tvar),
        bench "atomWrite" $ nfIO (atomWrite test_tvar "test2")
        ]]

        -- how do i test readAtomWait...?