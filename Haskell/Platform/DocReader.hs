module DocReader where

import Control.Monad
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Data.Text (Text, unpack)
import Pdf.Document
import Utils

-- Obs: you gotta use ':set -package text' and ':set -package deepseq' before loading

------------ External
data DocumentData = DocumentData {
                        name :: String,
                        n_tokens :: Int,
                        token_freq :: [(String, Int)]
                        }
                        deriving (Show)

tokenizeDoc :: MVar String -> String -> IO (String, [String])
tokenizeDoc thread_print filename = withPdfFile filename $ \pdf -> do
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
            -- Finishing thread
            putMVar thread_print $
                ("Reading " ++ filename ++ " (\n" ++ "    !! Failed cataloging document\n" ++ ");")
            return ("", [])

        Right catalog -> do
            rootNode <- catalogPageNode catalog
            count <- pageNodeNKids rootNode
            -- Tokenizing
            (text, tokenizePages_print) <- tokenizePages rootNode (count-1)
            let tokens = map clean_str (tokenizer $ show text)

            -- Finishing thread
            putMVar thread_print ("Reading " ++ filename ++ " (\n" ++ tokenizePages_print ++ ");")
            return (filename, tokens)

get_token_freq :: String -> DocumentData -> Int
get_token_freq token doc = do
        let (tkf:tkfs) = token_freq doc
        if token == (fst tkf) then snd tkf else get_token_freq token (
            DocumentData {name = (name doc), n_tokens = (n_tokens doc), token_freq = tkfs})

get_doc_data :: [String] -> MVar DocumentData -> IO (String, [String]) -> IO ()
get_doc_data tokens mvar io_data = do
    data_ <- io_data
    let content = snd data_
    let token_freq_ = zip tokens (map (token_frequency content) tokens)
    let len = length content

    (len, token_freq_) `deepseq` (putMVar mvar (DocumentData {
        name = (fst data_), n_tokens = len, token_freq = token_freq_}))

amount_of_documents_with :: String -> [DocumentData] -> Double
amount_of_documents_with _ [] = 0
amount_of_documents_with token (doc:docs) = (amount_of_documents_with token docs)
                                          + (if (get_token_freq token doc) > 0 then 1 else 0)

------------ Internal

type PdfText = IO Text

tokenizePages :: PageNode -> Int -> IO (String, String)
tokenizePages _ (-1) = return ("", "")
tokenizePages rootNode count = do
            (txt1, my_print) <- tokenizePages rootNode (count-1)
            page <- pageNodePageByNum rootNode count
            result <- try (pageExtractText page) :: IO (Either SomeException Text)
            case result of
                Left ex -> do
                    return (txt1, my_print ++ ("    !! Failed reading page " ++ (show count) ++ "\n"))
                Right val -> do
                    return $ (txt1 ++ (show val),
                        if ((count + 1) `mod` 100) == 0
                            then my_print ++ "    Read 100 Pages\n"
                            else my_print)