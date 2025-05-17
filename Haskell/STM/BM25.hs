module BM25 where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.DeepSeq
import DocReader (DocumentData, name, n_tokens, amount_of_documents_with, get_token_freq)
import Utils (atomWrite)

-- Obs: you gotta use ':set -package deepseq' before loading

------------ External

type Token = String
type DocumentText = [Token]

get_most_relevant_doc ::  [(String, Double)] -> (String, Double)
get_most_relevant_doc [] = ("", 0)
get_most_relevant_doc ((name, score) : xs) = do
    let best_doc = get_most_relevant_doc xs
    if score >= (snd best_doc) then (name, score) else best_doc

multithread_doc_score :: (Double, Double, [Double]) -> [Token] ->
                              (TVar (String, Double), DocumentData) -> IO ()
multithread_doc_score args query (result_TVar, doc) = do
    let score = doc_score args doc query
    score `deepseq` (atomWrite result_TVar (name doc, score))

-- Get average document length
get_avgdl :: Double -> [Int] -> Double
get_avgdl _ [] = 0
get_avgdl nDocs (freq:freqs) = (get_avgdl nDocs freqs) + (fromIntegral freq) / nDocs

-- Inverse document frequency
iDF :: Double -> [DocumentData] -> [Token] -> [Double]
iDF _ _ [] = []
iDF nDocs documents (token:tokens) = do
    let n = (amount_of_documents_with token documents)
    let idf = log (1 + (0.5 + nDocs - n)/(n + 0.5))
    idf : (iDF nDocs documents tokens)

------------ Internal

k = 1.5 :: Double
b = 0.75 :: Double

doc_score :: (Double, Double, [Double]) -> DocumentData -> [Token] -> Double
doc_score _ _ [] = 0
doc_score (nDocs, avgdl, idf:idfs) doc (token:tokens) = do
    let freq = fromIntegral (get_token_freq token doc)
    let doc_rate = (fromIntegral $ n_tokens doc)/avgdl
    let tail_score = doc_score (nDocs, avgdl, idfs) doc tokens
    let head_score = (idf * freq)/(freq + k*(1 + b*(-1 + doc_rate)))
    tail_score + head_score