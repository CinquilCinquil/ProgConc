module Main where

import Control.Monad
import Control.Concurrent
import System.Directory
import Pdf.Document
import DocReader
import BM25
import Utils

-- Obs: you gotta use ':set -package text' and ':set -package directory' before loading

query = tokenizer "partial" :: [Token]
filepath = "../../data/subset/" :: String

main = do
       ---- Gathering docs
       files <- getDirectoryContents filepath
       let pdf_names = filter (is_file_type "pdf") (map (filepath ++) files)
       let n_pdf_names = length pdf_names

       ---- Processing docs
       mvar_doc_data <- create_emtpy_mvars n_pdf_names :: IO [MVar DocumentData]
       thread_print <- newEmptyMVar

       let f = (forkIO . wrapper (get_doc_data query) (tokenizeDoc thread_print))
       threadIds <- mapM f (zip mvar_doc_data pdf_names)

       replicateM_ n_pdf_names (printMVar thread_print)
       -- waits until all threads finish
       doc_data_list <- mapM takeMVar mvar_doc_data :: IO [DocumentData]

       -- Filtering sucessfully processed docs
       let documents = filter (not_empty . name) doc_data_list :: [DocumentData]
       let n_processed_docs = length documents

       putStrLn $ "Processed " ++ (show n_processed_docs) ++ " out of " ++ (show $ n_pdf_names)

       ---- Calculating Score
       let nDocs = fromIntegral n_processed_docs :: Double
       let avgdl = get_avgdl nDocs (map n_tokens documents) :: Double
       let idfs = iDF nDocs documents query

       doc_score_mvars <- create_emtpy_mvars n_processed_docs :: IO [MVar (String, Double)]
       threadIds' <- mapM (forkIO . multithread_doc_score
              (nDocs, avgdl, idfs) query) (zip doc_score_mvars documents)

       doc_score_list <- mvar_list_to_list doc_score_mvars
       putStrLn $ fst $ (get_most_relevant_doc doc_score_list)