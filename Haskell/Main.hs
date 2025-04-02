module Main where

import Control.Monad
import System.Directory
import Pdf.Document
import DocReader
import BM25
import Utils

-- Obs: you gotta use ':set -package directory' before loading

query :: [Token] = tokenizer ("query")
filepath :: String = "../data/pdfs/"

main = do
       files <- getDirectoryContents filepath
       -- processing docs
       let pdf_files = filter (is_file_type "pdf") (map (filepath ++) files)
       pre_documents <- mapM tokenizeDoc pdf_files
       let documents = filter (\xs -> if xs == [] then True else False) pre_documents
       -- parameters
       let nDocs :: Double = fromIntegral (length documents)
       let avgdl :: Double = get_avgdl nDocs documents
       -- result
       let score = doc_score (nDocs, avgdl) documents (head documents) query
       print score

