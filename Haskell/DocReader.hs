module DocReader where

import Control.Monad
import Control.Exception
import Data.Text (Text, unpack)
import Pdf.Document
import Utils

-- Obs: you gotta use ':set -package text' before loading

------------ External

type PdfText = IO Text
--type PageNode = Pdf.Document.PageNode

tokenizeDoc :: String -> IO [String]
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
            return []
        Right catalog -> do
            rootNode <- catalogPageNode catalog
            count <- pageNodeNKids rootNode
            -- Tokenizing
            text <- tokenizePages rootNode (count-1)
            let tokens = map clean_str (tokenizer $ show text)
            putStrLn ");"
            return tokens

tokenizer :: String -> [String]
tokenizer "" = []
tokenizer (' ':[]) = []
tokenizer (' ':xs) = if ((head xs) == ' ') then tokenizer xs else []:(tokenizer xs)
tokenizer (x:xs) = conct_to_head x (tokenizer xs)

------------ Internal

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
                    when ((count `mod` 100) == 0) $ print "Read 100 Pages"
                    return $ txt1 ++ (show val)
