module Main where

import Control.Monad
import System.Directory
import Pdf.Document
import DocReader
import BM25

-- Obs: you gotta use ':set -package directory' before loading

query :: [Token] = tokenizer ("query")
filepath :: String = "../data/pdfs/"

main = do
       files <- getDirectoryContents filepath
       let pdf_files = filter (is_file_type "pdf") (map (filepath ++) files)
       documents <- wow $ map tokenizeDoc pdf_files
       let nDocs :: Double = fromIntegral (length documents)
       let avgdl :: Double = get_avgdl nDocs documents
       let score = doc_score (nDocs, avgdl) documents (head documents) query
       print score

wow :: [IO a] -> IO [a]
wow [] = return []
wow (x:xs) = do
             x' <- x
             xs' <- wow xs
             return (x': xs')

is_file_type :: String -> String -> Bool
is_file_type [] _ = True
is_file_type _ [] = False
is_file_type xs ys = fits_at_ys_start (reverse $ "." ++ xs) (reverse ys)
