module BM25 where

type Token = [Char]
type Document = [Token]

nDocs :: Double = 5
k :: Double = 1.5
b :: Double = 0.75
avgdl :: Double = 100
documents :: [Document] = [["oi", "3", "aaaa"], ["oi", "x", "wgompa"], []]

token_in_doc :: Token -> Document -> Bool
token_in_doc token [] = False
token_in_doc token (tk:doc) = if tk == token then True else (token_in_doc token doc)

amount_of_documents_with :: Token -> [Document] -> Double
amount_of_documents_with token [] = 0
amount_of_documents_with token (doc:docs) = (amount_of_documents_with token docs)
                                          + (if (token_in_doc token doc) then 1 else 0)

iDF :: Token -> Double
iDF qi = let nqi = (amount_of_documents_with qi documents) in
       log (1 + (0.5 + nDocs - nqi)/(nqi + 0.5))

token_freq :: Token -> Document -> Double
token_freq token [] = 0
token_freq token (tk:doc) = (token_freq token doc) + (if tk == token then 1 else 0)

doc_score :: Document -> [Token] -> Double
doc_score doc [] = 0
doc_score doc (x:xs) = let freq_in_doc = token_freq x doc in let doc_rate = ((fromIntegral $ length doc)/avgdl) in
                     (doc_score doc xs) + ((iDF x) * freq_in_doc)/(freq_in_doc + k*(1 - b*(1 + doc_rate)))