module Main where

import Control.Monad
import Pdf.Document
import DocReader
import BM25

-- falta ler mais de uma página
-- falta ler todos os docs de um arquivo

query :: [Token] = tokenizer ("enter your query here")

main = do
       tokens <- tokenizeDoc "input.pdf"
       let documents :: [BM25.Document] = [tokens]
       let nDocs :: Double = fromIntegral (length documents)
       let avgdl :: Double = get_avgdl nDocs documents
       let score = doc_score (nDocs, avgdl) documents (head documents) query
       print score