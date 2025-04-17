module DocReader where

import Control.Monad
import Control.Exception
import Data.Text (Text, unpack)
import Pdf.Document
import Utils

-- Obs: you gotta use ':set -package text' before loading

------------ External

data DocumentData = DocumentData {
                        name :: String,
                        n_tokens :: Int,
                        token_freq :: [(String, Int)]
                        }
                        deriving (Show)

tokenizeDoc :: String -> IO (String, [String])
tokenizeDoc filename = withPdfFile filename $ \pdf -> do
    putStrLn $ "Reading " ++ filename ++ " ("
    -- Dealing with encryption
    encrypted <- isEncrypted pdf
    when encrypted $ do
        ok <- setUserPassword pdf defaultUserPassword
        unless ok $ fail "need password"
    -- Getting info
    doc <- document pdf
    result <- try (documentCatalog doc) :: IO (Either SomeException Catalog)
    case result of 
        Left ex -> do
            putStrLn $
                "    !! Failed cataloging document"
            putStrLn ");"
            return ("", [])
        Right catalog -> do
            rootNode <- catalogPageNode catalog
            count <- pageNodeNKids rootNode
            -- Tokenizing
            text <- tokenizePages rootNode (count-1)
            let tokens = map clean_str (tokenizer $ show text)
            putStrLn ");"
            return (filename, tokens)

tokenizer :: String -> [String]
tokenizer "" = []
tokenizer (' ':[]) = []
tokenizer (' ':xs) = if ((head xs) == ' ') then tokenizer xs else []:(tokenizer xs)
tokenizer (x:xs) = conct_to_head x (tokenizer xs)

get_token_freq :: String -> DocumentData -> Int
get_token_freq token doc = do
        let (tkf:tkfs) = token_freq doc
        if token == (fst tkf) then snd tkf else get_token_freq token (
            DocumentData {name = (name doc), n_tokens = (n_tokens doc), token_freq = tkfs})

get_doc_data :: [String] -> IO (String, [String]) -> IO DocumentData
get_doc_data tokens io_data = do
    data_ <- io_data
    let content = snd data_
    let token_freq_ = zip tokens (map (token_frequency content) tokens)
    return (DocumentData {
        name = (fst data_), n_tokens = (length content), token_freq = token_freq_})

amount_of_documents_with :: String -> [DocumentData] -> Double
amount_of_documents_with _ [] = 0
amount_of_documents_with token (doc:docs) = (amount_of_documents_with token docs)
                                          + (if (get_token_freq token doc) > 0 then 1 else 0)

------------ Internal

type PdfText = IO Text

clean_str :: String -> String
clean_str s = remove_str "\\" $ remove_str "\"" $ (remove_sequence_of_str ["\\", "n"] s)

tokenizePages :: PageNode -> Int -> IO String
tokenizePages _ (-1) = return ""
tokenizePages rootNode count = do
            txt1 <- tokenizePages rootNode (count-1)
            page <- pageNodePageByNum rootNode count
            result <- try (pageExtractText page) :: IO (Either SomeException Text)
            case result of
                Left ex -> do
                    putStrLn $
                        "    !! Failed reading page " ++ (show count)
                    return txt1
                Right val -> do
                    when (((count + 1) `mod` 100) == 0) $ print "Read 100 Pages"
                    return $ txt1 ++ (show val)

token_frequency :: [String] -> String -> Int
token_frequency [] _ = 0
token_frequency (tk:doc) token = (token_frequency doc token) + (if tk == token then 1 else 0)