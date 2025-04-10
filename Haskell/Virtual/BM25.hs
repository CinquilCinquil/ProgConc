module BM25 where

import Control.Monad
import Control.Concurrent

------------ External

type Token = String
type Document = [Token]

get_most_relevant_doc :: (String, Double) -> [(String, Double)] -> (String, Double)
get_most_relevant_doc current_best [] = current_best
get_most_relevant_doc (best_name, best_score) ((name, score) : xs) = get_most_relevant_doc (
    if score >= best_score then (name, score) else (best_name, best_score)) xs

multithread_doc_score :: (Double, Double) -> [Document] -> [Token] ->
                              (MVar (String, Double), (String, Document)) -> IO ()
multithread_doc_score args doc_contents query (result_MVar, doc) = do
    let doc_name = fst doc
    let doc_content = snd doc
    let score = doc_score args doc_contents query doc_content
    when (score >= 0) $ do putMVar result_MVar (doc_name, score)

-- Get average document length
get_avgdl :: Double -> [Document] -> Double
get_avgdl _ [] = 0
get_avgdl nDocs (doc:docs) = (get_avgdl nDocs docs) + (fromIntegral (length doc)) / nDocs

------------ Internal

k = 1.5
b = 0.75

doc_score :: (Double, Double) -> [Document] -> [Token] -> Document -> Double
doc_score _ _ [] _ = 0
doc_score (nDocs, avgdl) documents (x:xs) doc =
    let freq_in_doc = token_freq x doc in
    let doc_rate = ((fromIntegral $ length doc)/avgdl) in
    (doc_score (nDocs, avgdl) documents xs doc) +
    ((iDF nDocs documents x) * freq_in_doc)/(freq_in_doc + k*(1 + b*(-1 + doc_rate)))

token_in_doc :: Token -> Document -> Bool
token_in_doc _ [] = False
token_in_doc token (tk:doc) = if tk == token then True else (token_in_doc token doc)

token_freq :: Token -> Document -> Double
token_freq _ [] = 0
token_freq token (tk:doc) = (token_freq token doc) + (if tk == token then 1 else 0)

amount_of_documents_with :: Token -> [Document] -> Double
amount_of_documents_with _ [] = 0
amount_of_documents_with token (doc:docs) = (amount_of_documents_with token docs)
                                          + (if (token_in_doc token doc) then 1 else 0)
-- Inverse document frequency
iDF :: Double -> [Document] -> Token -> Double
iDF nDocs documents qi = let nqi = (amount_of_documents_with qi documents) in
       log (1 + (0.5 + nDocs - nqi)/(nqi + 0.5))

