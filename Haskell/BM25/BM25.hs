module BM25 where

import Control.Concurrent

------------ External

type Token = String
type Document = [Token]

get_most_relevant_doc :: [MVar (String, Double)] -> IO (String, Double)
get_most_relevant_doc [] = return ("", -1)
get_most_relevant_doc (mvar_x : mvar_xs) = do

    x <- takeMVar mvar_x
    best_in_xs <- get_most_relevant_doc mvar_xs

    if (snd x) >= (snd best_in_xs) then
        return x
    else
        return best_in_xs

multithread_doc_score :: (Double, Double) -> [Document] -> [Token] ->
                              (MVar (String, Double), (String, Document)) -> IO ()
multithread_doc_score args doc_contents query (result_MVar, doc) = do
    let doc_name = fst doc
    let doc_content = snd doc
    putMVar result_MVar (doc_name, (doc_score args doc_contents query doc_content))

-- Get average document length
get_avgdl :: Double -> [Document] -> Double
get_avgdl _ [] = 0
get_avgdl nDocs (doc:docs) = (get_avgdl nDocs docs) + (fromIntegral (length doc)) / nDocs

------------ Internal

k :: Double = 1.5
b :: Double = 0.75

doc_score :: (Double, Double) -> [Document] -> [Token] -> Document -> Double
doc_score _ _ [] _ = 0
doc_score (nDocs, avgdl) documents (x:xs) doc =
    let freq_in_doc = token_freq x doc in
    let doc_rate = ((fromIntegral $ length doc)/avgdl) in
    (doc_score (nDocs, avgdl) documents xs doc) +
    ((iDF nDocs documents x) * freq_in_doc)/(freq_in_doc + k*(1 - b*(1 + doc_rate)))

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

