module DocReader where

import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.Text (Text, unpack)
import Pdf.Document
import Utils

-- Obs: you gotta use ':set -package text' before loading

------------ External
type NameAndDoc = (String, [String])
type PdfText = IO Text

tokenizeDoc :: MVar String -> (String, MVar NameAndDoc) -> IO ()
tokenizeDoc thread_print (filename, mvar) = withPdfFile filename $ \pdf -> do
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
            putMVar mvar ("", [])

        Right catalog -> do
            rootNode <- catalogPageNode catalog
            count <- pageNodeNKids rootNode
            -- Tokenizing
            (text, tokenizePages_print) <- tokenizePages rootNode (count-1)
            let tokens = map clean_str (tokenizer $ show text)

            -- Finishing thread
            putMVar thread_print ("Reading " ++ filename ++ " (\n" ++ tokenizePages_print ++ ");")
            putMVar mvar (filename, tokens)

tokenizer :: String -> [String]
tokenizer "" = []
tokenizer (' ':[]) = []
tokenizer (' ':xs) = if ((head xs) == ' ') then tokenizer xs else []:(tokenizer xs)
tokenizer (x:xs) = conct_to_head x (tokenizer xs)

------------ Internal

clean_str :: String -> String
clean_str s = remove_str "\\" $ remove_str "\"" $ (remove_sequence_of_str ["\\", "n"] s)

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
