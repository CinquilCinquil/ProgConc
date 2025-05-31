{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Criterion.Main
import System.Directory
import Control.DeepSeq (NFData(..))
import Control.Monad
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

main = do
    defaultMain [
        bgroup "DocReader" [
        bench "tokenizer" $ nf tokenizer test_text,
        bench "tokenizeDoc" $ nfIO $ tokenizeDoc test_filepath,
        bench "get_token_freq" $ nf (get_token_freq "out") test_docdata,
        bench "get_doc_data" $ nfIO (get_doc_data test_query test_io_data),
        bench "amount_of_documents_with" $ nf (amount_of_documents_with "in") test_documents,
        bench "clean_str" $ nf clean_str test_text,
        bench "token_frequency" $ nf (token_frequency test_tokens) "in"
        ],
        bgroup "BM25" [
        bench "get_most_relevant_doc" $ nf (get_most_relevant_doc test_params test_documents) test_query,
        bench "get_avgdl" $ nf (get_avgdl nDocs) [1000, 555, 431], -- TODO: increase? (on the others as well)
        bench "iDF" $ nf (iDF nDocs test_documents) test_query,
        bench "doc_score" $ nf (doc_score test_params test_docdata) test_query
        ],
        bgroup "Utils" [
        bench "is_file_type" $ nf (is_file_type test_filepath) "pdf",
        bench "conct_to_head" $ nf (conct_to_head 'a') ["bc", "def"],
        bench "fits_at_ys_start" $ nf (fits_at_ys_start "abc") "abcdef",
        bench "remove_str" $ nf (remove_str "de") "abcdef",
        bench "remove_sequence_of_str" $ nf (remove_sequence_of_str ["b", "de"]) "abcdef"
        ]]

--- copy of the main function with local variables
mainBench = do
        ---- Gathering docs
        files <- getDirectoryContents test_directory
        let pdf_names = filter (is_file_type "pdf") (map (test_directory ++) files)

        ---- Processing docs
        doc_data_list <- mapM (get_doc_data test_query . tokenizeDoc) pdf_names :: IO [DocumentData]

        let documents = filter (not_empty . name) doc_data_list :: [DocumentData]
        let n_processed_docs = length documents

        putStrLn $ "Processed " ++ (show n_processed_docs) ++ " out of " ++ (show $ length pdf_names)

        ---- Calculating Score

        let nDocs = fromIntegral n_processed_docs :: Double
        let avgdl = get_avgdl nDocs (map n_tokens doc_data_list) :: Double
        let idfs = iDF nDocs doc_data_list test_query

        putStrLn $ fst $ get_most_relevant_doc (nDocs, avgdl, idfs) doc_data_list test_query

-- use this to do a Macrobenchmark
main = do defaultMain [bench "mainBench" $ nfIO mainBench]