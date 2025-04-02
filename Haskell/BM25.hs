module BM25 where

------------ External

type Token = String
type Document = [Token]

get_most_relevant_doc :: (Double, Double) -> [(String, Document)] -> [Document] -> [Token] -> (String, Double)
get_most_relevant_doc _ [] _ _ = ("", 0)
get_most_relevant_doc params ((doc_name, document) : xs) docs_contents query =
    let score = doc_score params docs_contents document query in
    let most_relevant_doc_in_xs = get_most_relevant_doc params xs docs_contents query in
    let best_score_in_xs = snd $ most_relevant_doc_in_xs in
    if score >= best_score_in_xs
        then (doc_name, score)
        else most_relevant_doc_in_xs

-- Get average document length
get_avgdl :: Double -> [Document] -> Double
get_avgdl _ [] = 0
get_avgdl nDocs (doc:docs) = (get_avgdl nDocs docs) + (fromIntegral (length doc)) / nDocs

------------ Internal

k :: Double = 1.5
b :: Double = 0.75

doc_score :: (Double, Double) -> [Document] -> Document -> [Token] -> Double
doc_score _ _ _ [] = 0
doc_score (nDocs, avgdl) documents doc (x:xs) =
    let freq_in_doc = token_freq x doc in
    let doc_rate = ((fromIntegral $ length doc)/avgdl) in
    (doc_score (nDocs, avgdl) documents doc xs) +
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

